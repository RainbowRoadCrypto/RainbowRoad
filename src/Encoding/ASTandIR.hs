{-# LANGUAGE OverloadedStrings #-}

module Encoding.ASTandIR 
    ( astToText
    , astNodeToIR
    , irToText
    , paramToText
    , typeToText
    , operatorToText
    , literalToText
    , parameterToIRParam
    , typeNameToIRType
    , operationTypeToOperator
    , literalTypeToLiteral
    , typeNameToText
    , exampleASTConversion
    )
    where
import Types.Types 
    (Literal(..)
    , LiteralType(..)
    , ASTNode(..)
    , Operator(..)
    , Parameter(..)
    , IR(..)
    , Type(..)
    , FunctionName(..)
    , VariableName(..)
    , TypeName(..)
    , ASTParameter(..) -- Add this import
    , OperationType(..)) -- Add this import
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

astToText :: ASTNode -> Text
astToText = irToText . fromMaybe (IRUnknown "Invalid ASTNode") . astNodeToIR

-- Convert ASTNode to IR representation
astNodeToIR :: ASTNode -> Maybe IR
astNodeToIR astNode = case astNode of
    FunctionNode fname params body ->
        Just $ IRFunction fname (mapMaybe parameterToIRParam params) (mapMaybe astNodeToIR body)
    VariableDecl vname tname maybeNode ->
        Just $ IRVariableDecl vname (typeNameToIRType tname) (maybeNode >>= astNodeToIR)
    VariableAssign vname node ->
        Just $ IRVariableAssign vname (fromMaybe (IRUnknown "Invalid assignment") $ astNodeToIR node)
    OperationNode optype nodes ->
        Just $ IROperation (operationTypeToOperator optype) (mapMaybe astNodeToIR nodes)
    LiteralNode litType ->
        Just $ IRLiteral (literalTypeToLiteral litType)
    IfNode condition thenBranch elseBranch ->
        Just $ IRIf (fromMaybe (IRUnknown "Invalid condition") $ astNodeToIR condition)
                    (mapMaybe astNodeToIR thenBranch)
                    (fmap (mapMaybe astNodeToIR) elseBranch)
    WhileNode ltype cond body ->
        Just $ IRWhile (fromMaybe (IRUnknown "Invalid loop condition") $ astNodeToIR cond)
                        (mapMaybe astNodeToIR body)
    BlockNode nodes ->
        Just $ IRBlock (mapMaybe astNodeToIR nodes)
    FunctionCallNode fname args ->
        Just $ IRFunctionCall fname (mapMaybe astNodeToIR args)
    ClassNode tname members ->
        Just $ IRComment ("Class: " <> typeNameToText tname) -- Simplification for demonstration
    MethodNode fname params body ->
        Just $ IRFunction fname (mapMaybe parameterToIRParam params) (mapMaybe astNodeToIR body)
    ReturnNode maybeNode ->
        Just $ IRReturn (maybeNode >>= astNodeToIR)
    CommentNode text ->
        Just $ IRComment text
    _ -> Nothing -- Placeholder for unimplemented conversions

irToText :: IR -> Text
irToText (IRFunction (FunctionName fname) params body) = 
    "Function " <> fname <> "(" <> T.intercalate ", " (map paramToText params) <> "):\n" <> T.unlines (map irToText body)
irToText (IRVariableDecl (VariableName vname) vtype maybeInit) =
    "VarDecl " <> vname <> " : " <> typeToText vtype <> maybe "" ((" = " <>) . irToText) maybeInit
irToText (IRVariableAssign (VariableName vname) value) =
    "VarAssign " <> vname <> " = " <> irToText value
irToText (IROperation op operands) =
    "Operation " <> operatorToText op <> " [" <> T.intercalate ", " (map irToText operands) <> "]"
irToText (IRLiteral lit) =
    "Literal " <> literalToText lit
irToText (IRIf condition thenBranch elseBranch) =
    "If " <> irToText condition <> "\nThen:\n" <> T.unlines (map irToText thenBranch) <>
    maybe "" (\e -> "Else:\n" <> T.unlines (map irToText e)) elseBranch
irToText (IRWhile condition body) =
    "While " <> irToText condition <> "\nDo:\n" <> T.unlines (map irToText body)
irToText (IRBlock statements) =
    "Block:\n" <> T.unlines (map irToText statements)
irToText (IRFunctionCall (FunctionName fname) args) =
    "FunctionCall " <> fname <> "(" <> T.intercalate ", " (map irToText args) <> ")"
irToText (IRComment text) =
    "Comment: " <> text
irToText (IRSequence seq) =
    "Sequence:\n" <> T.unlines (map irToText seq)
irToText (IRUnknown text) =
    "Unknown: " <> text

paramToText :: Parameter -> Text
paramToText (Parameter (VariableName name) ptype) = name <> " : " <> typeToText ptype

typeToText :: Type -> Text
typeToText TypeInt = "Int"
typeToText TypeString = "String"
typeToText TypeBool = "Bool"
typeToText (TypeArray t) = "Array of " <> typeToText t
typeToText (TypeObject fields) = "Object {" <> T.intercalate ", " (map (\(VariableName name, t) -> name <> " : " <> typeToText t) fields) <> "}"
typeToText (TypeFunction params returnType) = "Function(" <> T.intercalate ", " (map typeToText params) <> ") : " <> typeToText returnType
typeToText TypeUnknown = "Unknown"

operatorToText :: Operator -> Text
operatorToText OpAdd = "+"
operatorToText OpSubtract = "-"
operatorToText OpMultiply = "*"
operatorToText OpDivide = "/"
operatorToText OpEqual = "=="
operatorToText OpNotEqual = "!="
operatorToText OpGreaterThan = ">"
operatorToText OpGreaterThanOrEqual = ">="
operatorToText OpLessThan = "<"
operatorToText OpLessThanOrEqual = "<="
operatorToText OpAnd = "&&"
operatorToText OpOr = "||"
operatorToText OpUnknown = "?"

literalToText :: Literal -> Text
literalToText (IntLit i) = T.pack $ show i
literalToText (StringLit s) = T.pack s
literalToText (BoolLit b) = if b then "true" else "false"
literalToText (ArrayLit l) = "[" <> T.intercalate ", " (map literalToText l) <> "]"
literalToText (ObjectLit fields) = "{" <> T.intercalate ", " (map (\(VariableName name, lit) -> name <> " : " <> literalToText lit) fields) <> "}"

parameterToIRParam :: ASTParameter -> Maybe Parameter
parameterToIRParam (ASTParameter (VariableName name) ptype) = Just $ Parameter (VariableName name) (typeNameToIRType ptype)

typeNameToIRType :: TypeName -> Type
typeNameToIRType (TypeName typeName) = case typeName of
    "Int" -> TypeInt
    "String" -> TypeString
    "Bool" -> TypeBool
    _ -> TypeUnknown

operationTypeToOperator :: OperationType -> Operator
operationTypeToOperator opType = case opType of
    OpArithmetic op -> case op of
        "Add" -> OpAdd
        "Subtract" -> OpSubtract
        "Multiply" -> OpMultiply
        "Divide" -> OpDivide
        _ -> OpUnknown
    OpLogical op -> case op of
        "And" -> OpAnd
        "Or" -> OpOr
        _ -> OpUnknown
    OpComparison op -> case op of
        "Equal" -> OpEqual
        "NotEqual" -> OpNotEqual
        "GreaterThan" -> OpGreaterThan
        "GreaterThanOrEqual" -> OpGreaterThanOrEqual
        "LessThan" -> OpLessThan
        "LessThanOrEqual" -> OpLessThanOrEqual
        _ -> OpUnknown
    OpBitwise op -> case op of
        "And" -> OpBitwiseAnd
        "Or" -> OpBitwiseOr
        "Xor" -> OpBitwiseXor
        "Not" -> OpBitwiseNot
        "ShiftLeft" -> OpShiftLeft
        "ShiftRight" -> OpShiftRight
        _ -> OpUnknown

    

literalTypeToLiteral :: LiteralType -> Literal
literalTypeToLiteral litType = case litType of
    LString text -> StringLit $ T.unpack text
    LNumber num -> IntLit $ floor num
    LBoolean bool -> BoolLit bool
    LNull -> StringLit "null"  -- Assuming you want to represent null as a string in your IR
    LTuple elems -> ArrayLit $ map literalTypeToLiteral elems
    LArray elems -> ArrayLit $ map literalTypeToLiteral elems
    LObject keyValPairs -> ObjectLit $ map (\(k, v) -> (VariableName $ T.pack $ T.unpack k, literalTypeToLiteral v)) keyValPairs

typeNameToText :: TypeName -> Text
typeNameToText (TypeName name) = name

-- Example demonstrating usage of AST to IR to Text conversion
exampleASTConversion :: Text
exampleASTConversion = astToText exampleFunctionAST
    where
    exampleFunctionAST = FunctionNode (FunctionName "example") [] [CommentNode "Example function"]
