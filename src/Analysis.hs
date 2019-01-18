module Analysis where

import CFG
import ConstraintSystem

import Data.Graph.Inductive.Graph
import Data.Maybe
import qualified Data.Set as S

basicExpr :: BasicOp -> S.Set Exp
basicExpr (Assign _ e) = S.singleton e
basicExpr (Load _ e) = S.singleton e
basicExpr (Store e1 e2) = S.fromList [e1, e2]
basicExpr Nop = S.empty

labExpr :: EdgeLabel -> S.Set Exp
labExpr (Pos e) = S.singleton e
labExpr (Neg e) = S.singleton e
labExpr (Basic bo) = basicExpr bo

edgeExpr :: CFGEdge -> S.Set Exp
edgeExpr (_, _, lab) = labExpr lab

allExpr :: CFG -> S.Set Exp
allExpr cfg = S.unions $ map edgeExpr (labEdges cfg)

startNodes :: CFG -> [Node]
startNodes g = map fst (filter f (labNodes g))
    where f (_, Start) = True
          f _ = False
endNodes :: CFG -> [Node]
endNodes g = map fst (filter f (labNodes g))
    where f (_, End) = True
          f _ = False

fvExp :: Exp -> S.Set Reg
fvExp (VarExp r) = S.singleton r
fvExp (Lit _) = S.empty
fvExp (Plus e1 e2) = fvExp e1 `S.union` fvExp e2
fvExp (Minus e1 e2) = fvExp e1 `S.union` fvExp e2
fvExp (Times e1 e2) = fvExp e1 `S.union` fvExp e2
fvExp (Less e1 e2) = fvExp e1 `S.union` fvExp e2
fvExp (Equal e1 e2) = fvExp e1 `S.union` fvExp e2
fvExp (Not e) = fvExp e

exprsWith :: S.Set Exp -> Reg -> S.Set Exp
exprsWith exprs v =
    S.filter (\x -> v `S.member` fvExp x) exprs

-- Available expression analysis, effect on edge
aeEffect :: S.Set Exp -> CFGEdge -> Constraint Exp Node
aeEffect allE (from, to, Pos e) = Constraint to (CUnion (CVar from) (CLit (S.singleton e)))
aeEffect allE (from, to, Neg e) = Constraint to (CUnion (CVar from) (CLit (S.singleton e)))
aeEffect allE (from, to, Basic Nop) = Constraint to (CVar from)
aeEffect allE (from, to, Basic (Assign r e)) =
    Constraint to (CMinus (CUnion (CVar from) (CLit (S.singleton e))) (CLit $ exprsWith allE r))
aeEffect allE (from, to, Basic (Load r e)) =
    Constraint to (CMinus (CUnion (CVar from) (CLit (S.singleton e))) (CLit $ exprsWith allE r))
aeEffect _ (from, to, Basic (Store e1 e2)) =
    Constraint to (CUnion (CVar from) (CLit (S.fromList [e1, e2])))

aeLattice :: S.Set Exp -> SetLattice Exp
aeLattice ae = SetLattice
    { incl = \x y -> S.isSubsetOf y x
    , bot = ae
    }

-- Collect all constraints
aeConstraints :: CFG -> CSystem Exp Node
aeConstraints g =
    let allE = allExpr g
        initC = map (\n -> Constraint n (CLit S.empty)) (startNodes g)
     in (map (aeEffect allE) (labEdges g)) ++ initC

-- Solve available expressions
getAe :: CFG -> CEnv Exp Node
getAe g =
    let lat = aeLattice (allExpr g)
     in solve lat (aeConstraints g)

tlLattice :: SetLattice Reg
tlLattice = SetLattice
    { incl = S.isSubsetOf
    , bot = S.empty
    }

-- True liveness
tlEffect :: CFGEdge -> Constraint Reg Node
tlEffect (from, to, Pos e) =
    Constraint from (CUnion (CVar to) (CLit (fvExp e)))
tlEffect (from, to, Neg e) =
    Constraint from (CUnion (CVar to) (CLit (fvExp e)))
tlEffect (from, to, Basic Nop) =
    Constraint from (CVar to)
tlEffect (from, to, Basic (Assign r e)) =
    Constraint from (CUnion (CMinus (CVar to) (CLit (S.singleton r)))
                            (CIncl r (CVar to) (fvExp e)))
tlEffect (from, to, Basic (Load r e)) =
    Constraint from (CUnion (CMinus (CVar to) (CLit (S.singleton r)))
                            (CIncl r (CVar to) (fvExp e)))
tlEffect (from, to, Basic (Store e1 e2)) =
    Constraint from (CUnion (CVar to) (CLit (fvExp e1 `S.union` fvExp e2)))

tlConstraints :: CFG -> CSystem Reg Node
tlConstraints g =
    let finalC = map (\n -> Constraint n (CLit S.empty)) (endNodes g)
     in map tlEffect (labEdges g) ++ finalC

-- Solve true liveness
getTl :: CFG -> CEnv Reg Node
getTl g = solve tlLattice (tlConstraints g)
