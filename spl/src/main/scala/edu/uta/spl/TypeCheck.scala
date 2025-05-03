package edu.uta.spl

import edu.uta.spl

abstract class TypeChecker {
  var trace_typecheck = false

  /** symbol table to store SPL declarations */
  var st = new SymbolTable

  def expandType(tp: Type): Type
  def typecheck(e: Expr): Type
  def typecheck(e: Lvalue): Type
  def typecheck(e: Stmt, expected_type: Type)
  def typecheck(e: Definition)
  def typecheck(e: Program)
}

class TypeCheck extends TypeChecker {

  /** typechecking error */
  def error(msg: String): Type = {
    System.err.println("*** Typechecking Error: " + msg)
    System.err.println("*** Symbol Table: " + st)
    System.exit(1)
    null
  }

  /** if tp is a named type, expand it */
  def expandType(tp: Type): Type =
    tp match {
      case NamedType(nm) =>
        st.lookup(nm) match {
          case Some(TypeDeclaration(t)) =>
            expandType(t)
          case _ => error("Undeclared type: " + tp)
        }
      case _ => tp
    }

  /** returns true if the types tp1 and tp2 are equal under structural equivalence */
  def typeEquivalence(tp1: Type, tp2: Type): Boolean =
    if (tp1 == tp2 || tp1.isInstanceOf[AnyType] || tp2.isInstanceOf[AnyType])
      true
    else
      expandType(tp1) match {
        case ArrayType(t1) =>
          expandType(tp2) match {
            case ArrayType(t2) =>
              typeEquivalence(t1, t2)
            case _ => false
          }
        case RecordType(fs1) =>
          expandType(tp2) match {
            case RecordType(fs2) =>
              fs1.length == fs2.length &&
                (fs1 zip fs2).map {
                  case (Bind(v1, t1), Bind(v2, t2)) =>
                    v1 == v2 && typeEquivalence(t1, t2)
                }.reduce(_ && _)
            case _ => false
          }
        case TupleType(ts1) =>
          expandType(tp2) match {
            case TupleType(ts2) =>
              ts1.length == ts2.length &&
                (ts1 zip ts2).map { case (t1, t2) => typeEquivalence(t1, t2) }
                  .reduce(_ && _)
            case _ => false
          }
        case _ =>
          tp2 match {
            case NamedType(n) => typeEquivalence(tp1, expandType(tp2))
            case _            => false
          }
      }

  /* tracing level */
  var level: Int = -1

  /** trace typechecking */
  def trace[T](e: Any, result: => T): T = {
    if (trace_typecheck) {
      level += 1
      println(" " * (3 * level) + "** " + e)
    }
    val res = result
    if (trace_typecheck) {
      print(" " * (3 * level))
      if (e.isInstanceOf[Stmt] || e.isInstanceOf[Definition])
        println("->")
      else println("-> " + res)
      level -= 1
    }
    res
  }

  /** typecheck an expression AST */
  def typecheck(e: Expr): Type =
    trace(
      e,
      e match {
        case BinOpExp(op, l, r) =>
          val ltp = typecheck(l)
          val rtp = typecheck(r)
          if (!typeEquivalence(ltp, rtp))
            error("Incompatible types in binary operation: " + e)
          else if (op.equals("and") || op.equals("or"))
            if (typeEquivalence(ltp, BooleanType()))
              ltp
            else error("AND/OR operation can only be applied to booleans: " + e)
          else if (op.equals("eq") || op.equals("neq"))
            BooleanType()
          else if (!typeEquivalence(ltp, IntType()) && !typeEquivalence(ltp, FloatType()))
            error(
              "Binary arithmetic operations can only be applied to integer or real numbers: " + e
            )
          else if (
            op.equals("gt") || op.equals("lt") || op.equals("geq") || op.equals("leq")
          )
            BooleanType()
          else ltp

        /* PUT YOUR CODE HERE */

        case IntConst(value) =>
          IntType()

        case FloatConst(value) =>
          FloatType()

        case BooleanConst(value) =>
          BooleanType()

        case StringConst(value) =>
          StringType()

        case LvalExp(lvalue) =>
          typecheck(lvalue)

        case UnOpExp(operator, operand) =>
          val operandType = typecheck(operand)
          operator match {
            case "minus" =>
              if (
                !typeEquivalence(operandType, IntType()) && !typeEquivalence(
                  operandType,
                  FloatType()
                )
              )
                error(
                  "Unary minus operator can only be applied to integer or real numbers: " + e
                )
              else
                operandType
            case "not" =>
              if (!typeEquivalence(operandType, BooleanType()))
                error("Unary not operator can only be applied to booleans: " + e)
              else
                operandType
            case _ =>
              error("Unknown unary operator: " + operator)
          }

        case NullExp() =>
          AnyType()

        case CallExp(funcName, args) =>
          st.lookup(funcName) match {
            case Some(FuncDeclaration(retType, params, _, _, _)) =>
              if (args.length != params.length)
                error(
                  s"Function '$funcName' expects ${params.length} arguments, but ${args.length} were provided."
                )
              else {
                (args zip params).foreach { case (argExpr, Bind(_, paramType)) =>
                  val argType = typecheck(argExpr)
                  if (!typeEquivalence(argType, paramType))
                    error(
                      s"Argument type $argType does not match expected parameter type $paramType in function '$funcName'."
                    )
                }
                retType
              }
            case Some(_) =>
              error(s"'$funcName' is not a function.")
            case None =>
              error(s"Undefined function: $funcName")
          }

        case ArrayGen(sizeExpr, elemExpr) =>
          val sizeType = typecheck(sizeExpr)
          if (!typeEquivalence(sizeType, IntType()))
            error("Array size must be of integer type: " + sizeExpr)
          val elemType = typecheck(elemExpr)
          ArrayType(elemType)

        case ArrayExp(elems) =>
          if (elems.isEmpty) {
            error("Array expressions cannot be empty.")
            ArrayType(NoType())
          } else {
            val firstElemType = typecheck(elems.head)
            elems.tail.foreach { elem =>
              val elemType = typecheck(elem)
              if (!typeEquivalence(elemType, firstElemType))
                error(
                  s"Inconsistent element types in array. Expected $firstElemType but found $elemType."
                )
            }
            ArrayType(firstElemType)
          }

        case TupleExp(elems) =>
          val elemTypes = elems.map(typecheck)
          TupleType(elemTypes)

        case RecordExp(fields) =>
          val fieldTypeList = fields.map {
            case Bind(fieldName, fieldExpr) =>
              val fieldType = typecheck(fieldExpr)
              Bind(fieldName, fieldType)
          }
          RecordType(fieldTypeList)

        case _ => throw new Error("Wrong expression: " + e)
      }
    )

  /** typecheck an Lvalue AST */
  def typecheck(e: Lvalue): Type =
    trace(
      e,
      e match {
        case Var(variableName) =>
          st.lookup(variableName) match {
            case Some(VarDeclaration(varType, _, _)) => varType
            case Some(_)                             => error(variableName + " is not a variable")
            case None                                => error("Undefined variable: " + variableName)
          }

        /* PUT YOUR CODE HERE */

        case RecordDeref(rec, attr) =>
          val recType = typecheck(rec)
          expandType(recType) match {
            case RecordType(fields) =>
              fields.find(_.name == attr) match {
                case Some(Bind(_, attrType)) => attrType
                case None =>
                  error(s"Attribute '$attr' not found in record type: " + recType)
              }
            case _ => error(s"Expected a record type for RecordDeref but found: " + recType)
          }

        case TupleDeref(tup, idx) =>
          val tupType = typecheck(tup)
          expandType(tupType) match {
            case TupleType(types) =>
              if (idx >= 0 && idx < types.length)
                types(idx)
              else
                error(s"Index $idx out of bounds for tuple type: " + tupType)
            case _ => error(s"Expected a tuple type for TupleDeref but found: " + tupType)
          }

        case ArrayDeref(arr, idxExpr) =>
          val arrType = typecheck(arr)
          val idxType = typecheck(idxExpr)
          arrType match {
            case ArrayType(elemType) =>
              if (!typeEquivalence(idxType, IntType()))
                error("Array index must be an integer: " + idxExpr)
              else
                elemType
            case _ => error("Array dereference on non-array type: " + arrType)
          }

        case _ => throw new Error("Wrong lvalue: " + e)
      }
    )

  /** typecheck a statement AST using the expected type of the return value from the current function */
  def typecheck(e: Stmt, expected_type: Type) {
    trace(
      e,
      e match {
        case AssignSt(lhs, rhs) =>
          val lhsType = typecheck(lhs)
          val rhsType = typecheck(rhs)
          if (!typeEquivalence(lhsType, rhsType))
            error("Incompatible types in assignment: " + e)

        /* PUT YOUR CODE HERE */

        case CallSt(funcName, args) =>
          st.lookup(funcName) match {
            case Some(FuncDeclaration(retType, params, _, _, _)) =>
              if (args.length != params.length)
                error(
                  s"Function '$funcName' expects ${params.length} arguments, but ${args.length} were provided."
                )
              else {
                (args zip params).foreach { case (argExpr, Bind(_, paramType)) =>
                  val argType = typecheck(argExpr)
                  if (!typeEquivalence(argType, paramType))
                    error(
                      s"Argument type $argType does not match expected parameter type $paramType in function '$funcName'."
                    )
                }
                retType
              }
            case Some(_) =>
              error(s"'$funcName' is not a function.")
            case None =>
              error(s"Undefined function: $funcName")
          }

        case ReadSt(vars) =>
          vars.foreach(typecheck)

        case PrintSt(exprs) =>
          exprs.foreach(typecheck)

        case IfSt(cond, thenStmt, elseStmt) =>
          val condType = typecheck(cond)
          if (!typeEquivalence(condType, BooleanType()))
            error("Expected a boolean in IF statement: " + cond)
          typecheck(thenStmt, expected_type)
          if (elseStmt != null)
            typecheck(elseStmt, expected_type)

        case WhileSt(cond, body) =>
          val condType = typecheck(cond)
          if (!typeEquivalence(condType, BooleanType()))
            error("Expected a boolean in WHILE statement: " + cond)
          typecheck(body, expected_type)

        case LoopSt(body) =>
          typecheck(body, NoType())

        case ForSt(varName, initExpr, stepExpr, incExpr, body) =>
          st.begin_scope()
          st.insert(varName, VarDeclaration(IntType(), 0, 0))
          val initType = typecheck(initExpr)
          if (!typeEquivalence(initType, IntType()))
            error("Initial value in FOR loop must be integer: " + initExpr)
          val stepType = typecheck(stepExpr)
          if (!typeEquivalence(stepType, IntType()))
            error("Step in FOR loop must be integer: " + stepExpr)
          typecheck(incExpr)
          typecheck(body, expected_type)
          st.end_scope()

        case ExitSt() =>
          // nothing to do

        case ReturnValueSt(retExpr) =>
          val retType = typecheck(retExpr)
          if (!typeEquivalence(retType, expected_type))
            error(
              s"Return type $retType does not match expected type $expected_type in function."
            )

        case ReturnSt() =>
          if (!typeEquivalence(AnyType(), expected_type))
            error("Return statement without value in function expecting type: " + expected_type)

        case BlockSt(definitions, statements) =>
          st.begin_scope()
          definitions.foreach(typecheck)
          statements.foreach(stmt => typecheck(stmt, expected_type))
          st.end_scope()

        case _ => throw new Error("Wrong statement: " + e)
      }
    )
  }

  /** typecheck a definition */
  def typecheck(e: Definition) {
    trace(
      e,
      e match {

        case TypeDef(typeName, typeDef) =>
          st.insert(typeName, TypeDeclaration(typeDef))

        case FuncDef(funcName, params, returnType, body) =>
          st.insert(funcName, FuncDeclaration(returnType, params, "", 0, 0))
          st.begin_scope()
          params.foreach { case Bind(paramName, paramType) =>
            st.insert(paramName, VarDeclaration(paramType, 0, 0))
          }
          typecheck(body, returnType)
          st.end_scope()

        case VarDef(varName, varType, expr) =>
          val exprType = typecheck(expr)
          val inferredType = varType match {
            case AnyType() => exprType
            case _         => varType
          }
          if (!typeEquivalence(inferredType, exprType)) {
            error("Incompatible types in variable declaration: " + e)
          } else {
            st.insert(varName, VarDeclaration(inferredType, 0, 0))
          }

        /* PUT YOUR CODE HERE */

        case _ => throw new Error("Wrong statement: " + e)
      }
    )
  }

  /** typecheck the main program */
  def typecheck(e: Program) {
    typecheck(e.body, NoType())
  }
}
