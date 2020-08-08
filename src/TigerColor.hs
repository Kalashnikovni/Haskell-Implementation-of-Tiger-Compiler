module TigerColor where

import TigerAssem
import TigerErrores
import TigerFrame as F
import TigerLiveness as L
import TigerMakeGraph as FG
import TigerSymbol
import TigerTemp as T
import TigerUnique

import Algebra.Graph.AdjacencyMap as Adj

import Control.Conditional as C
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Trans.Except

import Data.List as List
import Data.Map as M
import Data.Set as S
import Data.Stack as Stk
import qualified Data.Text.Lazy as Lazy

import Debug.Trace

import Prelude as P

import System.Directory

-- La key es temporario de TigerTemp
-- El valor es registro de TigerFrame
type Allocation = Map Temp Temp 

precolored :: Allocation
precolored = M.fromList $ zip allregs allregs
  --where p = [fp, sp, lo, hi, zero, ra, rv0, rv1, gp, a0, a1, a2, a3]

kNumber :: Int
kNumber = List.length allregs

spillCost :: ATemp -> Int
spillCost node = 1 

build :: (Color w) => [Instr] -> w ()
build instrs = 
  do let (f, v) = instrs2graph instrs
         (fg, vs) = i2gWithJumps instrs (f, v) v
         forBuild = reverse $ M.toList $ inout $ snd $ runState (setEquationAlgorithm fg vs) initLEstado 
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
     let kPrec = keys precolored
     mapM_ (\p -> do ig <- getIGraph
                     putIGraph ig{edgesG = P.map (\e -> (p, e)) (kPrec List.\\ [p]) ++ 
                                           edgesG ig,
                                  adjSet = P.foldl (\set e -> S.insert (e, p) $ S.insert (p, e) set) 
                                                   (adjSet ig) 
                                                   (kPrec List.\\ [p])}) kPrec 
     igraph <- getIGraph
     let res = edges $ edgesG igraph
     putIGraph igraph{graph = res, 
                      bitMatrix = buildBitMatrix res}                                
     return ()

buildInstruction :: (Color w) => Instr -> w ()
buildInstruction instr@(Move ass dst src) =
  do useC <- getUse 
     live1 <- getLive
     putLive (live1 S.\\ useC) 
     defC <- getDef
     mapM_ (\n -> do moveL <- getMoveList
                     maybe (putMoveList $ M.insert n (S.singleton instr) moveL)
                           (\v -> putMoveList $ M.insert n (S.union v (S.singleton instr)) moveL)
                           (M.lookup n moveL)) (S.union defC useC)
     addWorkListMoves instr    
     live2 <- getLive
     putLive (S.union live2 defC)
     live3 <- getLive
     mapM_ (\d -> mapM_ (\l -> addEdge l d) live3) defC
     putLive $ S.union useC (live3 S.\\ defC)
     return ()
buildInstruction instr = 
  do defC <- getDef
     live1 <- getLive
     putLive (S.union live1 defC)
     live2 <- getLive
     mapM_ (\d -> mapM_ (\l -> addEdge l d) live2) defC
     useC <- getUse
     putLive $ S.union useC (live2 S.\\ defC)
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
       Nothing -> internalAux $ "TigerColor 8" -- ++ show node ++ show deg 

nodeMoves :: (Color w) => ATemp -> w (Set Instr)
nodeMoves node =
  do moveL <- getMoveList
     actMoves <- getActiveMoves
     workLMoves <- getWorkListMoves 
     maybe (return $ S.empty)
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
     maybe (internalAux $ "TigerColor 11")
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
    x <- getAlias mSrc
    y <- getAlias mDst
    case S.member y $ keysSet precolored of
      True -> coalesceAux m y x 
      False -> coalesceAux m x y

getDst :: (Color w) => Instr -> w ATemp
getDst (Move ass [] src) = internalAux $ "TigerColor 22" ++ " " ++ show ass ++ " " ++ show src
getDst (Move ass dst src) = return $ dst !! 0 
getDst _ = internalAux "TigerColor 15"

getSrc :: (Color w) => Instr -> w ATemp
getSrc (Move ass _ []) = internalAux $ "TigerColor 21" ++ " " ++ show ass
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
                               putSimplifyWorkList $ S.union simplWL uSet
                    False -> return ())
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
     let newAdj = edges $ edgesG ig
     putIGraph ig{graph = newAdj}
     ig' <- getIGraph
     freezeWL <- getFreezeWorkList
     maybe (internalAux $ "TigerColor 20" ++ show u ++ show (degree ig'))
           (\d -> ifM (return $ d >= kNumber && S.member u freezeWL) 
                      (do let uSet = S.singleton u
                          putFreezeWorkList $ freezeWL S.\\ uSet
                          spillWL <- getSpillWorkList
                          putSpillWorkList $ S.union spillWL uSet)
                      (return ()))
           (M.lookup u $ degree ig)

freeze :: (Color w) => w ()
freeze = 
  do freezeWL <- getFreezeWorkList
     let u = S.elemAt 0 freezeWL
     let uSet = S.singleton u
     putFreezeWorkList (freezeWL S.\\ uSet)
     simplifyWL <- getSimplifyWorkList
     putSimplifyWorkList (S.union simplifyWL uSet)
     freezeMoves u

freezeMoves :: (Color w) => ATemp -> w () 
freezeMoves u =
  do nodeMvs <- nodeMoves u 
     mapM_ (freezeMovesAux u) (S.toList nodeMvs)

freezeMovesAux :: (Color w) => ATemp -> Instr -> w ()
freezeMovesAux u m =       
  do mDst <- getDst m
     mSrc <- getSrc m
     x <- getAlias mSrc
     y <- getAlias mDst
     uAlias <- getAlias u
     let v = if y == u then x else y
     let mSet = S.singleton m
     actMoves <- getActiveMoves
     putActiveMoves (actMoves S.\\ mSet)
     frozenMoves <- getFrozenMoves
     putFrozenMoves (S.union frozenMoves mSet)
     vNodeMoves <- nodeMoves v
     ig <- getIGraph
     maybe (internalAux "TigerColor 23")
           (\d -> case S.null vNodeMoves && d < kNumber of
                    True -> do let vSet = S.singleton v
                               freezeWL <- getFreezeWorkList
                               putFreezeWorkList (freezeWL S.\\ vSet)
                               simplifyWL <- getSimplifyWorkList
                               putSimplifyWorkList (S.union simplifyWL vSet)
                    False -> return ())  
           (M.lookup v $ degree ig)

selectSpill :: (Color w) => w ()
selectSpill =
  do m <- pickSpill
     let mSet = S.singleton m
     spillWL <- getSpillWorkList
     putSpillWorkList (spillWL S.\\ mSet)
     simplifyWL <- getSimplifyWorkList
     putSimplifyWorkList (S.union simplifyWL mSet)
     freezeMoves m     

-- TODO: mejorar esta heuristica
pickSpill :: (Color w) => w ATemp
pickSpill =
  do spillWL <- getSpillWorkList
     return $ S.elemAt 0 spillWL

assignColors :: (Color w) => w ()
assignColors =
  do whileM_ (do selectStk <- getSelectStack
                 return $ not $ stackIsEmpty selectStk)
             (do selectStk <- getSelectStack
                 maybe (internalAux $ "TigerColor 24 -- Haskell error")
                       (\(newStk, n) -> 
                         do putSelectStack newStk
                            let okColors = S.fromList usablecolors
                            ig <- getIGraph
                            let adjList = graph ig
                            let wSet = S.union (preSet n adjList) (postSet n adjList)
                            nAlias <- getAlias n
                            newOkColors <- limitColors wSet okColors
                            let nSet = S.singleton n
                            case S.null newOkColors of
                              True -> do spilled <- getSpilledNodes
                                         putSpilledNodes $ S.union spilled nSet
                              False -> do colored <- getColoredNodes
                                          putColoredNodes $ S.union colored nSet
                                          let c = S.elemAt 0 newOkColors
                                          color <- getColor
                                          putColor $ M.insert n c color)
                       (stackPop selectStk))
     coalesced <- getCoalescedNodes
     paintCoalesced coalesced

limitColors :: (Color w) => Set ATemp -> Set ATemp -> w (Set ATemp)
limitColors wSet colors 
  | S.null wSet = return colors
  | otherwise = do let w = S.elemAt 0 wSet
                   wAlias <- getAlias w
                   colored <- getColoredNodes
                   color <- getColor
                   let newSet = wSet S.\\ (S.singleton w)
                   let newColors = case S.member wAlias (S.union colored (keysSet precolored)) of
                                     True -> colors S.\\ (maybe (S.empty)
                                                                (\res -> S.singleton res)
                                                                (M.lookup wAlias color))
                                     False -> colors
                   limitColors newSet newColors

paintCoalesced :: (Color w) => Set ATemp -> w ()
paintCoalesced coal 
  | S.null coal = return ()
  | otherwise = do let n = S.elemAt 0 coal
                   let newCoal = coal S.\\ (S.singleton n)
                   color <- getColor
                   nAlias <- getAlias n
                   maybe (paintCoalesced newCoal)
                         (\c -> do putColor $ M.insert n c color
                                   paintCoalesced newCoal)
                         (M.lookup nAlias color)

rewriteProgram :: (Color w) => w ()
rewriteProgram = 
  do spilled <- getSpilledNodes 
     u <- getUse
     d <- getDef
     let sz = wSz * S.size spilled
     instrs <- getInstrs
     putInstrs $ [head instrs] ++ 
                 [Oper{assem = "addq $-"  ++ show sz ++ ", `d0\n", dst = [sp], src = [], jump = Nothing}] ++
                 (tail instrs)
     putSpillAlloc M.empty
     newTemps <- rewriteSpilled
     fr <- getFrame
     putFrame fr{actualReg = actualReg fr + S.size newTemps}
     putSpilledNodes S.empty
     colored <- getColoredNodes
     coalesced <- getCoalescedNodes
     putInit $ S.union (S.union colored coalesced) newTemps 
     putColoredNodes S.empty
     putCoalescedNodes S.empty

rewriteSpilled :: (Color w) => w (Set ATemp)
rewriteSpilled =
  do instrs <- getInstrs 
     newInstrsTemps <- mapM rewriteSpilledAux instrs
     putInstrs $ concat $ P.map fst newInstrsTemps
     return $ S.unions $ P.map snd newInstrsTemps

rewriteSpilledAux :: (Color w) => Instr -> w ([Instr], Set ATemp) 
rewriteSpilledAux (Oper assem dst src jmp) =
  do spilled <- getSpilledNodes
     let sDst = S.fromList dst
     let spillDst = S.toList $ S.intersection sDst spilled
     createNewTemps spillDst
     stores <- mapM createStore spillDst
     let sSrc = S.fromList src
     let spillSrc = S.toList $ S.intersection (S.fromList src) spilled 
     createNewTemps spillSrc
     loads <- mapM createLoad spillSrc
     newSrc <- mapM replaceTemp src
     newDst <- mapM replaceTemp dst 
     let newTemps = (S.union (S.fromList newSrc) (S.fromList newDst)) S.\\
                    (S.union sDst sSrc)
     return $ (loads ++ [Oper assem newDst newSrc jmp] ++ stores, 
               newTemps)
rewriteSpilledAux (Move assem dst src) =
  do spilled <- getSpilledNodes
     let sDst = S.fromList dst
     let spillDst = S.toList $ S.intersection sDst spilled
     createNewTemps spillDst
     stores <- mapM createStore spillDst
     let sSrc = S.fromList src
     let spillSrc = S.toList $ S.intersection sSrc spilled 
     createNewTemps spillSrc
     loads <- mapM createLoad spillSrc
     newSrc <- mapM replaceTemp src
     newDst <- mapM replaceTemp dst 
     let newTemps = (S.union (S.fromList newSrc) (S.fromList newDst)) S.\\
                    (S.union sDst sSrc)
     return $ (loads ++ [Move assem newDst newSrc] ++ stores, 
               newTemps)
rewriteSpilledAux (ILabel assem lab) = return ([ILabel assem lab], S.empty)

createNewTemps :: (Color w) => [ATemp] -> w ()
createNewTemps [] = return ()
createNewTemps (t:ts) =
  do sAlloc <- getSpillAlloc 
     case M.member t sAlloc of
       True -> createNewTemps ts
       False -> do t' <- newTemp 
                   off <- TigerColor.getOffset
                   putSpillAlloc $ M.insert t (t', off) sAlloc
                   putOffset $ off + 1
                   createNewTemps ts

replaceTemp :: (Color w) => ATemp -> w ATemp
replaceTemp t = 
  do sAlloc <- getSpillAlloc
     return $ maybe t
                    fst 
                    (M.lookup t sAlloc) 

createStore :: (Color w) => ATemp -> w Instr
createStore t =
  do sAlloc <- getSpillAlloc
     let (newT, off) = maybe (P.error "TigerColor 30") id (M.lookup t sAlloc) 
     return Oper{assem = "movq `s0, " ++ show (off * (-wSz)) ++ "(`s1)\n",
                 dst = [], src = [newT, fp], jump = Nothing}  

createLoad :: (Color w) => ATemp -> w Instr
createLoad t =
  do sAlloc <- getSpillAlloc
     let (newT, off) = maybe (P.error "TigerColor 31") id (M.lookup t sAlloc)
     return Oper{assem = "movq " ++  show (off * (-wSz)) ++ "(`s0), `d0\n",
                 dst = [newT], src = [fp], jump = Nothing}

replace :: (Color w, Eq a) => a -> a -> [a] -> w [a]
replace _ _ [] = return []
replace old new (a:as)
  | old == a  = 
      do res <- replace old new as
         return $ new : res
  | otherwise =
      do res <- replace old new as
         return $ a : res

mainColor :: (Color w) => Frame -> w ()     
mainColor fr =
  do instrs <- getInstrs
     init <- getInit
     putIGraph IG{degree = M.union (M.fromList $ P.map (\i -> (i, 0)) (S.toList init))
                                   (M.fromList $ P.map (\p -> (p, max32Int)) (keys precolored)),
                  edgesG = [],
                  adjSet = S.empty}
     build instrs
     makeWorkList
     untilM (do simplifyWL <- getSimplifyWorkList
                ifM (return $ not $ S.null simplifyWL) 
                    simplify
                    (do wlMoves <- getWorkListMoves
                        ifM (return $ not $ S.null wlMoves)
                            coalesce
                            (do freezeWL <- getFreezeWorkList
                                ifM (return $ not $ S.null freezeWL)
                                    freeze
                                    (do spillWL <- getSpillWorkList
                                        ifM (return $ not $ S.null spillWL)
                                            selectSpill
                                            (return ())))))
            (do simplifyWL <- getSimplifyWorkList
                wlMoves <- getWorkListMoves
                freezeWL <- getFreezeWorkList
                spillWL <- getSpillWorkList
                return $ S.null simplifyWL && S.null wlMoves && S.null freezeWL && S.null spillWL)   
     assignColors
     spilled <- getSpilledNodes
     coal <- getCoalescedMoves
     ifM (return $ not $ S.null spilled)
         (do rewriteProgram
             mainColor fr)
         (return ())

registerAllocation :: (Color w) => [Instr] -> Frame -> w ([Instr], Frame)
registerAllocation [] fr = return ([], fr)
registerAllocation instrs fr =
  do putInstrs instrs
     putFrame fr
     putInit $ (P.foldl (\set i -> S.union (makeInit i) set) (S.empty) instrs) S.\\ (keysSet precolored)
     putOffset $ locSize fr + 2
     mainColor fr
     applyColors
     newInstrs <- getInstrs
     fram <- getFrame
     newIG <- getIGraph
     return (removeCoal newInstrs, fram)

applyColors :: (Color w) => w ()
applyColors = 
  do instrs <- getInstrs
     alloc <- getColor 
     putInstrs $ P.map (applyColor alloc) instrs

applyColor :: Allocation -> Instr -> Instr
applyColor alloc (Oper assem dst src jmp) = 
  let newDst = P.map (\t -> maybe (P.error "TigerColor 26")
                                  id
                                  (M.lookup t alloc)) dst
      newSrc = P.map (\t -> maybe (P.error "TigerColor 27")
                                  id
                                  (M.lookup t alloc)) src
  in Oper assem newDst newSrc jmp
applyColor alloc (Move assem dst src) = 
  let newDst = P.map (\t -> maybe (P.error "TigerColor 28")
                                  id
                                  (M.lookup t alloc)) dst
      newSrc = P.map (\t -> maybe (P.error "TigerColor 29")
                                  id
                                  (M.lookup t alloc)) src
  in Move assem newDst newSrc
applyColor alloc (ILabel assem lab) = ILabel assem lab

removeCoal :: [Instr] -> [Instr]
removeCoal [] = [] 
removeCoal (Oper assem dst src jmp : is) =
  Oper assem dst src jmp : (removeCoal is)
removeCoal (Move assem dst src : is) =
  if dst == src then removeCoal is else (Move assem dst src : (removeCoal is))
removeCoal (ILabel assem lab : is) = ILabel assem lab : removeCoal is
    

makeInit :: Instr -> Set ATemp
makeInit (Oper assem dst src jmp) = S.union (S.fromList dst) (S.fromList src)
makeInit (Move assem dst src) = S.union (S.fromList dst) (S.fromList src)
makeInit _ = S.empty

runMonadaRA :: AllocMonada ([Instr], Frame) -> StGen (Either Symbol ([Instr], Frame), AllocEstado)
runMonadaRA mon = runStateT (runExceptT mon) initAlloc 

data AllocEstado = AE{initial :: Set ATemp,
                      simplifyWorkList :: Set ATemp,
                      freezeWorkList :: Set ATemp,
                      spillWorkList :: Set ATemp,
                      spilledNodes :: Set ATemp,
                      coalescedNodes :: Set ATemp,
                      coloredNodes :: Set ATemp,
                      color :: Allocation,
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
                      alias :: Map ATemp ATemp,
                      stInstrs :: [Instr],
                      offset :: Int,
                      spillAlloc :: Map ATemp (ATemp, Int),
                      frame :: Frame}


initAlloc :: AllocEstado 
initAlloc = AE{initial = S.empty,
               simplifyWorkList = S.empty,
               freezeWorkList = S.empty,
               spillWorkList = S.empty,
               spilledNodes = S.empty,
               coalescedNodes = S.empty,
               coloredNodes = S.empty,
               color = precolored,
               selectStack = stackNew,
               coalescedMoves = S.empty,
               constrainedMoves = S.empty,
               frozenMoves = S.empty,
               workListMoves = S.empty,
               activeMoves = S.empty,
               interfGraph = defaultIGraph,
               moveList = M.empty,
               live = S.empty,
               TigerColor.use = S.empty,
               TigerColor.def = S.empty,
               alias = M.empty,
               stInstrs = [],
               offset = 0,
               spillAlloc = M.empty, 
               frame = defaultFrame}

type AllocMonada = ExceptT Symbol (StateT AllocEstado StGen)

class (Demon w, Monad w, UniqueGenerator w, TLGenerator w) => Color w where
  getSimplifyWorkList :: w (Set ATemp)
  putSimplifyWorkList :: Set ATemp -> w ()
  getFreezeWorkList :: w (Set ATemp) 
  putFreezeWorkList :: Set ATemp -> w ()
  getSpillWorkList :: w (Set ATemp) 
  putSpillWorkList :: Set ATemp -> w ()
  getSpilledNodes :: w (Set ATemp)
  putSpilledNodes :: Set ATemp -> w ()
  getCoalescedNodes :: w (Set ATemp)
  putCoalescedNodes :: Set ATemp -> w ()
  getColoredNodes :: w (Set ATemp)
  putColoredNodes :: Set ATemp -> w ()
  getColor :: w Allocation
  putColor :: Allocation -> w ()
  getSelectStack :: w (Stack ATemp)
  putSelectStack :: Stack ATemp -> w ()
  getCoalescedMoves :: w (Set Instr)
  putCoalescedMoves :: Set Instr -> w ()
  getConstrainedMoves :: w (Set Instr)
  putConstrainedMoves :: Set Instr -> w ()
  getFrozenMoves :: w (Set Instr)
  putFrozenMoves :: Set Instr -> w ()
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
  getInstrs :: w [Instr]
  putInstrs :: [Instr] -> w ()
  getSpillAlloc :: w (Map ATemp (ATemp, Int))
  putSpillAlloc :: Map ATemp (ATemp, Int) -> w ()
  getOffset :: w Int
  putOffset :: Int -> w ()
  getFrame :: w Frame
  putFrame :: Frame ->  w ()

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
  getSpilledNodes =
    do st <- get
       return $ spilledNodes st
  putSpilledNodes nodes = 
    do st <- get
       put st{spilledNodes = nodes}
  getCoalescedNodes =
    do st <- get
       return $ coalescedNodes st
  putCoalescedNodes nodes = 
    do st <- get
       put st{coalescedNodes = nodes}
  getColoredNodes =
    do st <- get
       return $ coloredNodes st
  putColoredNodes nodes =
    do st <- get
       put $ st{coloredNodes = nodes}
  getColor =
    do st <- get
       return $ color st
  putColor newColor =
    do st <- get
       put $ st{color = newColor}
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
  getFrozenMoves =
    do st <- get
       return $ frozenMoves st
  putFrozenMoves mvs =
    do st <- get
       put st{frozenMoves = mvs}
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
       let g = graph igraph
       let adjSet' = S.insert (l, d) $ S.insert (d, l) $ adjSet igraph
       C.when (not $ List.elem l precolored) 
              (do let edgesG' = (l, d) : edgesG igraph
                  maybe (putIGraph igraph{adjSet = adjSet',
                                          edgesG = edgesG',
                                          degree = M.insert l 0 (degree igraph)})
                        (\degree' -> putIGraph igraph{adjSet = adjSet',
                                                      edgesG = edgesG',
                                                      degree = M.insert l (degree' + 1) (degree igraph)})
                        (M.lookup l $ degree igraph))
       C.when (not $ List.elem d precolored) 
              (do let edgesG' = (d, l) : edgesG igraph
                  maybe (putIGraph igraph{adjSet = adjSet',
                                          edgesG = edgesG',
                                          degree = M.insert d 0 (degree igraph)})
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
  getInstrs =
    do st <- get
       return $ stInstrs st
  putInstrs instrs =
    do st <- get
       put st{stInstrs = instrs}
  getOffset =
    do st <- get
       return $ offset st
  putOffset off =
    do st <- get
       put st{offset = off}
  getSpillAlloc = 
    do st <- get
       return $ spillAlloc st
  putSpillAlloc sAlloc =
    do st <- get
       put st{spillAlloc = sAlloc}
  getFrame =
    do st <- get
       return $ frame st
  putFrame fr =
    do st <- get
       put st{frame = fr}
