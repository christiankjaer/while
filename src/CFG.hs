module CFG where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

import Control.Monad.State

type Reg = String

data Exp = VarExp Reg
         | Lit Integer
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Less Exp Exp
         | Equal Exp Exp
         | Not Exp
         deriving (Eq, Show)

data BasicOp = Assign Reg Exp
             | Load Reg Exp
             | Store Reg Exp
             | Nop
             deriving (Eq, Show)

data EdgeLabel = Pos Exp
               | Neg Exp
               | Basic BasicOp
               deriving (Eq, Show)


-- Basic While language with memory
data Stmt = BasicStmt BasicOp
          | IfStmt Exp Stmt Stmt
          | SeqStmt Stmt Stmt
          | WhileStmt Exp Stmt
          deriving (Eq, Show)

data NodeType = Start | End | Normal deriving (Eq, Show)

type CFGNode = LNode NodeType
type CFGEdge = LEdge EdgeLabel

type CFG = Gr NodeType EdgeLabel

startGraph :: CFG
startGraph = mkGraph [(0, Start), (1, End)] []

type CompileState = (Node, CFG)

compile :: Stmt -> CFG
compile s = snd . snd $ runState (compileStmt s 0 1) (2, startGraph)


insertNode :: State CompileState Node
insertNode = do
    (nextNode, cfg) <- get
    let newCfg = insNode (nextNode, Normal) cfg
    put (nextNode+1, newCfg)
    return nextNode

insertEdge :: CFGEdge -> State CompileState ()
insertEdge e = do
    (nextNode, cfg) <- get
    put (nextNode, insEdge e cfg)
    return ()

compileStmt :: Stmt -> Node -> Node -> State CompileState ()

compileStmt (SeqStmt s1 s2) start end = do
    n <- insertNode
    compileStmt s1 start n
    compileStmt s2 n end

compileStmt (IfStmt e pos neg) start end = do
    n1 <- insertNode
    n2 <- insertNode
    insertEdge (start, n1, Pos e)
    insertEdge (start, n2, Neg e)
    compileStmt pos n1 end
    compileStmt neg n2 end

compileStmt (WhileStmt e body) start end = do
    startBody <- insertNode
    endBody <- insertNode
    insertEdge (start, end, Neg e)
    insertEdge (start, startBody, Pos e)
    insertEdge (endBody, start, Basic Nop)
    compileStmt body startBody endBody

compileStmt (BasicStmt b) start end =
    insertEdge (start, end, Basic b)
