module ConstraintSystem where

import qualified Data.Set as S
import qualified Data.Map as M

type Var = String

data CTerm a b = CUnion (CTerm a b) (CTerm a b)
             | CInt (CTerm a b) (CTerm a b)
             | CMinus (CTerm a b) (CTerm a b)
             | CLit (S.Set a)
             | CVar b
             deriving (Show, Eq, Ord)

data Constraint a b = Constraint b (CTerm a b) deriving (Eq, Show, Ord)

type CSystem a b = [Constraint a b]

type CEnv a b = M.Map b (S.Set a)

-- Free variables
fv :: Ord b => CTerm a b -> S.Set b
fv (CVar v) = S.singleton v
fv (CLit _) = S.empty
fv (CMinus t1 t2) = fv t1 `S.union` fv t2
fv (CUnion t1 t2) = fv t1 `S.union` fv t2
fv (CInt t1 t2) = fv t1 `S.union` fv t2

-- Evaluate a set expression
ceval :: Ord a => Ord b => CEnv a b -> CTerm a b -> S.Set a
ceval _ (CLit s) = s
ceval env (CVar v) = M.findWithDefault S.empty v env
ceval env (CUnion t1 t2) = ceval env t1 `S.union` ceval env t2
ceval env (CInt t1 t2) = ceval env t1 `S.intersection` ceval env t2
ceval env (CMinus t1 t2) = ceval env t1 `S.difference` ceval env t2

type InfluenceMap a b = M.Map b (S.Set (Constraint a b))

-- Figure out which constraints are influenced by a variable.
infl :: Ord a => Ord b => CSystem a b -> InfluenceMap a b
infl cs =
    let xs = [(x', S.singleton c) | c@(Constraint _ t) <- cs, x' <- S.toList (fv t)]
     in M.fromListWith S.union xs


testCs :: CSystem String Var
testCs = [ Constraint "x1" (CUnion (CLit (S.singleton "a")) (CVar "x3"))
         , Constraint "x2" (CInt (CVar "x3") (CLit (S.fromList ["a", "b"])))
         , Constraint "x3" (CUnion (CVar "x1") (CLit (S.singleton "c")))
         ]

initAnalysis :: Ord b => SetLattice a -> CSystem a b -> CEnv a b
initAnalysis lat cs = M.fromList (map (\(Constraint v _) -> (v, bot lat)) cs)


data SetLattice a = SetLattice
    { incl :: S.Set a -> S.Set a -> Bool
    , bot :: S.Set a
    }

solve :: Ord a => Ord b => SetLattice a -> CSystem a b -> CEnv a b
solve lat cs = iter cs (initAnalysis lat cs)
    where
        iter [] env = env
        iter (Constraint x t : cs') env =
            let new = ceval env t
                infls = S.toList (M.findWithDefault S.empty x (infl cs))
             in if not (incl lat new (env M.! x))
                   then iter (cs' ++ infls) (M.insert x new env)
                   else iter cs' env
