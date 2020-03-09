module TigerColor where

import TigerAssem
import TigerErrores
import TigerFrame as F
import TigerLiveness as L
import TigerMakeGraph as FG
import TigerSymbol
import TigerTemp as T

import Algebra.Graph.AdjacencyMap as Adj

import Control.Conditional as C
import Control.Monad.State
import Control.Monad.Trans.Except

import Data.List as List
import Data.Map as M
import Data.Set as S
import Data.Stack as Stk

import Prelude as P

-- La key es temporario de TigerTemp
-- El valor es registro de TigerFrame
type Allocation = Map Temp Temp 

-- 
precolored :: Allocation
precolored = M.fromList $ zip p p
  where p = [fp, sp, lo, hi, zero, ra, rv0, rv1, gp, a0, a1, a2, a3]

kNumber :: Int
kNumber = List.length allregs

data AllocEstado = AE{initial :: Set ATemp,
                      simplifyWorkList :: Set ATemp,
                      freezeWorkList :: Set ATemp,
                      spillWorkList :: Set ATemp,
                      spilledNodes :: Set ATemp,
                      coalescedNodes :: Set ATemp,
                      coloredNodes :: Set ATemp,
                      selectStack :: Stack ATemp,
                      coalescedMoves :: Set Instr,
                      constrainedMoves :: Set Instr,
                      frozenMoves :: Set Instr,
                      workListMoves :: Set Instr,
                      activeMoves :: Set Instr,
                      interfGraph :: IGraph,
                      moveList :: Map ATemp (Set Instr),
                      live :: Set ATemp,
                      use :: Set ATemp,
                      def :: Set ATemp,
                      alias :: Map ATemp ATemp}

type AllocMonada = ExceptT Symbol (State AllocEstado)

class (Demon w, Monad w) => Color w where
  getSimplifyWorkList :: w (Set ATemp)
  putSimplifyWorkList :: Set ATemp -> w ()
  getFreezeWorkList :: w (Set ATemp) 
  putFreezeWorkList :: Set ATemp -> w ()
  getSpillWorkList :: w (Set ATemp) 
  putSpillWorkList :: Set ATemp -> w ()
  getCoalescedNodes :: w (Set ATemp)
  putCoalescedNodes :: Set ATemp -> w ()
  getSelectStack :: w (Stack ATemp)
  putSelectStack :: Stack ATemp -> w ()
  getCoalescedMoves :: w (Set Instr)
  putCoalescedMoves :: Set Instr -> w ()
  getConstrainedMoves :: w (Set Instr)
  putConstrainedMoves :: Set Instr -> w ()
  getWorkListMoves :: w (Set Instr)
  addWorkListMoves :: Instr -> w ()
  putWorkListMoves :: Set Instr -> w ()
  getActiveMoves :: w (Set Instr) 
  putActiveMoves :: Set Instr -> w () 
  getIGraph :: w IGraph
  putIGraph :: IGraph -> w ()
  makePutIGraph :: ATemp -> ATemp -> w ()
  getInit :: w (Set ATemp)
  putInit :: Set ATemp -> w ()
  getMoveList :: w (Map ATemp (Set Instr))
  addMoveList :: Set Instr -> ATemp -> w () 
  putMoveList :: Map ATemp (Set Instr) -> w ()
  getLive :: w (Set ATemp) 
  putLive :: Set ATemp -> w ()
  getUse :: w (Set ATemp)
  putUse :: Set ATemp -> w ()
  getDef :: w (Set ATemp)
  putDef :: Set ATemp -> w ()
  getAliasW :: w (Map ATemp ATemp)
  putAliasW :: Map ATemp ATemp -> w ()

instance Demon AllocMonada where
  derror      =  throwE . pack . (++ "\n") . unpack 
  adder w msg = withExceptT (\e -> addStr (unpack msg) e) w 

instance Color AllocMonada where
  getSimplifyWorkList =
    do st <- get
       return $ simplifyWorkList st
  putSimplifyWorkList nodes =
    do st <- get
       put st{simplifyWorkList = nodes}
  getFreezeWorkList =
    do st <- get
       return $ freezeWorkList st
  putFreezeWorkList nodes =
    do st <- get
       put st{freezeWorkList = nodes}
  getSpillWorkList =
    do st <- get
       return $ spillWorkList st
  putSpillWorkList nodes = 
    do st <- get
       put st{spillWorkList = nodes}
  getCoalescedNodes =
    do st <- get
       return $ coalescedNodes st
  putCoalescedNodes nodes = 
    do st <- get
       put st{coalescedNodes = nodes}
  getSelectStack =
    do st <- get
       return $ selectStack st
  putSelectStack stk = 
    do st <- get
       put st{selectStack = stk}
  getCoalescedMoves =
    do st <- get
       return $ coalescedMoves st
  putCoalescedMoves mvs = 
    do st <- get
       put st{coalescedMoves = mvs}
  getConstrainedMoves =
    do st <- get
       return $ constrainedMoves st
  putConstrainedMoves mvs = 
    do st <- get
       put st{constrainedMoves = mvs}
  getWorkListMoves =
    do st <- get
       return $ workListMoves st
  addWorkListMoves instr =
    do st <- get
       put st{workListMoves = S.insert instr $ workListMoves st}
  putWorkListMoves mvs = 
    do st <- get
       put st{workListMoves = mvs}
  getActiveMoves = 
    do st <- get
       return $ activeMoves st
  putActiveMoves instrs =
    do st <- get
       put st{activeMoves = instrs}
  getIGraph =
    do st <- get
       return $ interfGraph st
  putIGraph igraph =
    do st <- get
       put $ st{interfGraph = igraph}
  makePutIGraph l d =
    do st <- get
       let igraph = interfGraph st
       let adjSet' = S.insert (l, d) $ S.insert (d, l) $ adjSet igraph
       C.when (not $ List.elem l precolored) 
              (do let edgesG' = (l, d) : edgesG igraph
                  maybe (internalAux "TigerColor 4")
                        (\degree' -> putIGraph igraph{adjSet = adjSet',
                                                      edgesG = edgesG',
                                                      degree = M.insert l (degree' + 1) (degree igraph)})
                        (M.lookup l $ degree igraph))
       C.when (not $ List.elem d precolored) 
              (do let edgesG' = (d, l) : edgesG igraph
                  maybe (internalAux "TigerColor 5")
                        (\degree' -> putIGraph igraph{adjSet = adjSet',
                                                      edgesG = edgesG',
                                                      degree = M.insert d (degree' + 1) (degree igraph)})
                        (M.lookup d $ degree igraph))
       return ()
  getInit =
    do st <- get
       return $ initial st 
  putInit nodes = 
    do st <- get
       put st{initial = nodes}
  getMoveList =
    do st <- get
       return $ moveList st
  addMoveList instrs t =
    do st <- get
       let moveL = moveList st
       maybe (internal $ pack "TigerColor 7")
             (\actInstrs -> put st{moveList = M.insert t (S.union actInstrs instrs) moveL})
             (M.lookup t moveL)
  putMoveList newMap =
    do st <- get
       put st{moveList = newMap} 
  getLive = 
    do st <- get
       return $ live st
  putLive setLive = 
    do st <- get
       put st{live = setLive}
  getUse = 
    do st <- get
       return $ TigerColor.use st
  putUse setUse = 
    do st <- get
       put st{TigerColor.use = setUse}
  getDef = 
    do st <- get
       return $ TigerColor.def st
  putDef setDef = 
    do st <- get
       put st{TigerColor.def = setDef}
  getAliasW = 
    do st <- get
       return $ alias st
  putAliasW newMap =
    do st <- get
       put st{alias = newMap}

spillCost :: ATemp -> Int
spillCost node = 1 

build :: (Color w) => [Instr] -> w ()
build instrs = 
  do let (fg, vs) = instrs2graph instrs
     let forBuild = M.toList $ inout $ snd $ runState (setEquationAlgorithm fg vs) initLEstado 
     mapM_ (\(node, (_, liveOut)) -> 
              do putLive liveOut
                 case (M.lookup node $ FG.use fg, M.lookup node $ FG.def fg) of
                   (Just fguse, Just fgdef) -> 
                     do putUse $ S.fromList fguse
                        putDef $ S.fromList fgdef
                        case M.lookup node $ info fg of
                          Nothing -> internalAux "TigerColor 1"
                          Just i  -> buildInstruction i
                   _ -> internalAux "TigerColor 2") forBuild
     igraph <- getIGraph
     let res = edges $ edgesG igraph
     putIGraph igraph{graph = res, bitMatrix = buildBitMatrix res}                                
     return ()

buildInstruction :: (Color w) => Instr -> w ()
buildInstruction instr@(Move ass dst src) =
  do use <- getUse 
     live <- getLive
     putLive (live S.\\ use) 
     def <- getDef
     mapM_ (addMoveList $ S.singleton instr) (S.union def use)
     addWorkListMoves instr    
     live <- getLive
     putLive (S.union live def)
     live <- getLive
     mapM_ (\d -> mapM_ (\l -> addEdge l d) live) def
     return ()
buildInstruction instr = 
  do def <- getDef
     live <- getLive
     putLive (S.union live def)
     live <- getLive
     mapM_ (\d -> mapM_ (\l -> addEdge l d) live) def
     return ()

addEdge :: (Color w) => ATemp -> ATemp -> w ()
addEdge l d = 
  do igraph <- getIGraph
     case l /= d && (not $ elem (l, d) $ edgesG igraph) of
       True -> makePutIGraph l d 
       False -> return () 

makeWorkList :: (Color w) => w ()
makeWorkList = 
  do init <- getInit
     sequence_ $ List.map mkWorkListAux (S.toList init) 

mkWorkListAux :: (Color w) => ATemp -> w ()
mkWorkListAux node = 
  do init <- getInit
     putInit (init S.\\ (S.singleton node))  
     ig <- getIGraph
     let deg = degree ig
     let nodeSet = S.singleton node
     case M.lookup node deg of
       Just d -> case d >= kNumber of
                   True -> do spillWL <- getSpillWorkList
                              putSpillWorkList (S.union spillWL nodeSet)
                   False -> do nMoves <- nodeMoves node
                               case S.null nMoves of
                                 True -> do simplifyWL <- getSimplifyWorkList
                                            putSimplifyWorkList (S.union simplifyWL nodeSet)
                                 False -> do freezeWL <- getFreezeWorkList
                                             putFreezeWorkList (S.union freezeWL nodeSet) 
       Nothing -> internalAux "TigerColor 8"  

nodeMoves :: (Color w) => ATemp -> w (Set Instr)
nodeMoves node =
  do moveL <- getMoveList
     actMoves <- getActiveMoves
     workLMoves <- getWorkListMoves 
     maybe (internalAux "TigerColor 6")
           (\instrs -> return $ S.intersection instrs (S.union actMoves workLMoves))
           (M.lookup node moveL) 

simplify :: (Color w) => w ()
simplify = 
  do simplifyWL <- getSimplifyWorkList 
     case S.null simplifyWL of
       True -> derrorAux "TigerColor 9 -- No nodes left to simplify"
       False -> do let node = S.elemAt 0 simplifyWL
                   putSimplifyWorkList (simplifyWL S.\\ (S.singleton node))  
                   selectStk <- getSelectStack  
                   putSelectStack (stackPush selectStk node) 
                   adjs <- adjacent node
                   mapM_ decrementDegree adjs

adjacent :: (Color w) => ATemp -> w (Set ATemp)  
adjacent node =
  do ig <- getIGraph
     let adjList = graph ig
     let adjListn = S.union (preSet node adjList) (postSet node adjList) 
     selectStk <- getSelectStack 
     coalesced <- getCoalescedNodes 
     return $ adjListn S.\\ (S.union (stkToSet selectStk) coalesced)

stkToSet :: (Ord a) => Stack a -> Set a
stkToSet stk 
  | stackIsEmpty stk = S.empty
  | otherwise = maybe (error "TigerColor 10 -- Haskell error")
                      (\(newStk, res) -> S.insert res (stkToSet newStk))
                      (stackPop stk)

decrementDegree :: (Color w) => ATemp -> w ()
decrementDegree m = 
  do ig <- getIGraph
     let deg = degree ig
     maybe (internalAux "TigerColor 11")
           (\d -> do putIGraph ig{degree = M.insert m (d - 1) deg}
                     let mSet = S.singleton m
                     case d == kNumber of
                       False -> return ()
                       True  -> do adjs <- adjacent m
                                   enableMoves $ S.union mSet adjs
                                   spillWL <- getSpillWorkList
                                   putSpillWorkList (spillWL S.\\ mSet)
                                   nodeMvs <- nodeMoves m
                                   case S.null nodeMvs of
                                     True -> do simplifyWL <- getSimplifyWorkList
                                                putSimplifyWorkList (S.union simplifyWL mSet)
                                     False -> do freezeWL <- getFreezeWorkList
                                                 putFreezeWorkList (S.union freezeWL mSet))
           (M.lookup m deg) 

enableMoves :: (Color w) => Set ATemp -> w () 
enableMoves nodes = 
  mapM_ (\node -> do moves <- nodeMoves node
                     mapM_ (\m -> do actMoves <- getActiveMoves
                                     let mSet = S.singleton m
                                     case S.member m actMoves of
                                       True -> do putActiveMoves (actMoves S.\\ mSet)
                                                  addWorkListMoves m 
                                       False -> return ()) 
                           moves) nodes

coalesce :: (Color w) => w ()
coalesce =
 do wlMoves <- getWorkListMoves 
    let m = S.elemAt 0 wlMoves 
    putWorkListMoves (wlMoves S.\\ (S.singleton m))
    mDst <- getDst m
    mSrc <- getSrc m
    x <- getAlias mDst
    y <- getAlias mSrc
    case S.member y $ keysSet precolored of
      True -> coalesceAux m y x 
      False -> coalesceAux m x y

getDst :: (Color w) => Instr -> w ATemp
getDst (Move _ _ []) = internalAux "TigerColor 22"
getDst (Move ass dst src) = return $ dst !! 0 
getDst _ = internalAux "TigerColor 15"

getSrc :: (Color w) => Instr -> w ATemp
getSrc (Move _ _ []) = internalAux "TigerColor 21"
getSrc (Move ass dst src) = return $ src !! 0 
getSrc _ = internalAux "TigerColor 17"

coalesceAux :: (Color w) => Instr -> ATemp -> ATemp -> w ()
coalesceAux m u v =
  do ig <- getIGraph
     let pre = keysSet precolored
     let mSet = S.singleton m
     case u == v of 
       True -> 
         do coalesced <- getCoalescedMoves
            putCoalescedMoves (S.union coalesced mSet)
            addWorkList u 
       False -> 
         case S.member v pre || S.member (u, v) (adjSet ig) of
           True -> do mvs <- getConstrainedMoves
                      putConstrainedMoves (S.union mvs mSet)
                      addWorkList u
                      addWorkList v
           False -> 
             do adjV <- adjacent v
                adjU <- adjacent u
                pred1 <- mapM (\t -> ok t u) (S.toList adjV)
                pred2 <- conservative 0 $ S.union adjU adjV
                case (S.member u pre && and pred1) || ((not $ S.member u pre) && pred2) of
                  True -> 
                    do coalesced <- getCoalescedMoves 
                       putCoalescedMoves (S.union coalesced mSet)
                       combine u v
                       addWorkList u
                  False -> 
                    do actMoves <- getActiveMoves
                       putActiveMoves (S.union actMoves mSet)

addWorkList :: (Color w) => ATemp -> w ()
addWorkList u =
  do let uSet = S.singleton u
     let notPrecolored = not $ S.member u $ keysSet precolored  
     mvInstrs <- nodeMoves u 
     let condMoves = S.null mvInstrs
     ig <- getIGraph
     maybe (internalAux "TigerColor 12")
           (\d -> case d < kNumber && notPrecolored && condMoves of
                    True -> do freezeWL <- getFreezeWorkList
                               putFreezeWorkList $ freezeWL S.\\ uSet
                               simplWL <- getSimplifyWorkList
                               putSimplifyWorkList $ S.union simplWL uSet)
           (M.lookup u $ degree ig)

ok :: (Color w) => ATemp -> ATemp -> w Bool
ok t r = 
  do ig <- getIGraph
     maybe (internalAux "TigerColor 13")
           (\d -> return $ d < kNumber || S.member t (keysSet precolored) || S.member (t, r) (adjSet ig))
           (M.lookup t $ degree ig)

conservative :: (Color w) => Int -> Set ATemp -> w Bool
conservative k nodes
  | S.null nodes = return $ k < kNumber
  | otherwise = 
      do ig <- getIGraph
         let n = S.elemAt 0 nodes
         let newNodes = nodes S.\\ (S.singleton n)
         maybe (internalAux "TigerColor 14")
               (\d -> if d >= kNumber then conservative (k + 1) newNodes
                        else conservative k newNodes)
               (M.lookup n $ degree ig)

getAlias :: (Color w) => ATemp -> w ATemp
getAlias n =
  do coalesced <- getCoalescedNodes
     case S.member n coalesced of 
       True -> do aliases <- getAliasW
                  maybe (internalAux "TigerColor 15")
                        (\newN -> getAlias newN)
                        (M.lookup n aliases)
       False -> return n

combine :: (Color w) => ATemp -> ATemp -> w ()
combine u v = 
  do let vSet = S.singleton v
     freezeWL <- getFreezeWorkList
     ifM (return $ S.member v freezeWL) 
         (putFreezeWorkList $ freezeWL S.\\ vSet)
         (do spillWL <- getSpillWorkList
             putSpillWorkList $ spillWL S.\\ vSet)
     coalesced <- getCoalescedNodes
     putCoalescedNodes $ S.union coalesced vSet
     aliases <- getAliasW 
     putAliasW $ M.insert v u aliases
     mvList <- getMoveList
     res <- maybe (internalAux "TigerColor 18")
                  (\mvU -> maybe (internalAux "TigerColor 19")
                                 (\mvV -> return $ S.union mvU mvV)
                                 (M.lookup v mvList))
                  (M.lookup u mvList)
     putMoveList $ M.insert u res mvList
     enableMoves vSet -- FIXME: esto es asi?
     adjV <- adjacent v
     mapM_ (\t -> do addEdge t u
                     decrementDegree t) adjV 
     ig <- getIGraph 
     freezeWL <- getFreezeWorkList
     maybe (internalAux "TigerColor 20")
           (\d -> ifM (return $ d >= kNumber && S.member u freezeWL) 
                      (do let uSet = S.singleton u
                          putFreezeWorkList $ freezeWL S.\\ uSet
                          spillWL <- getSpillWorkList
                          putSpillWorkList $ S.union spillWL uSet)
                      (return ()))
           (M.lookup u $ degree ig)
     
{-color :: IGraph -> Allocation -> (Vertex -> Int) -> [Temp] -> (Allocation, [Temp])
color ig precolored ??? allregs 
-}
