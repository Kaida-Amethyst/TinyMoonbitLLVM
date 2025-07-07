///|
pub suberror CodeGenError String derive(Show)

///|
pub struct CodeGen {
  parser_prog : @parser.Program
  llvm_ctx : @llvm.Context
  llvm_prog : @llvm.Module
  builder : @llvm.Builder
  struct_tys : Map[String, @llvm.StructType]
  llvm_funcs : Map[String, @llvm.FunctionValue]
}

///|
pub fn CodeGen::init(parser_prog : @parser.Program) -> CodeGen {
  let llvm_ctx = @llvm.Context::create()
  let llvm_prog = llvm_ctx.create_module("tinymoonbit")
  let builder = llvm_ctx.create_builder()
  let struct_tys = Map::new()
  let llvm_funcs = Map::new()
  CodeGen::{ parser_prog, llvm_ctx, llvm_prog, builder, struct_tys, llvm_funcs }
}

///|
pub fn CodeGen::dump(self : Self) -> Unit {
  self.llvm_prog.dump()
}

///|
traitalias @llvm.Type

///|
fn CodeGen::convert_parser_type_to_llvm_type(
  self : Self,
  ty : @parser.Type
) -> &Type raise {
  let ctx = self.llvm_ctx
  match ty {
    Unit => {
      ctx.void_type() as &Type
    }
    Bool => ctx.bool_type()
    Int | UInt => ctx.i32_type()
    Int64 | UInt64 => ctx.i64_type()
    Float => ctx.f32_type()
    Double => ctx.f64_type()
    Ptr(_) => ctx.ptr_type()
    Array(_) => ctx.ptr_type()
    Struct(sname) =>
      match self.struct_tys.get(sname) {
        Some(struct_ty) => struct_ty
        None => raise CodeGenError("Error: Struct type not found: \{sname}")
      }
  }
}

///|
fn CodeGen::convert_parser_type_to_llvm_basic_type(
  self : Self,
  ty : @parser.Type
) -> &@llvm.BasicType raise {
  let ctx = self.llvm_ctx
  match ty {
    Bool => {
      ctx.bool_type() as &@llvm.BasicType
    }
    Int | UInt => ctx.i32_type()
    Int64 | UInt64 => ctx.i64_type()
    Float => ctx.f32_type()
    Double => ctx.f64_type()
    Ptr(_) => ctx.ptr_type()
    Array(_) => ctx.ptr_type()
    Struct(sname) =>
      match self.struct_tys.get(sname) {
        Some(struct_ty) => {
          struct_ty as &@llvm.BasicType
        }
        None => raise CodeGenError("Error: Struct type not found: \{sname}")
      }
    Unit =>
      raise CodeGenError("Error: Cannot convert Unit type to LLVM basic type")
  }
}

///|
fn CodeGen::getFunction(self : Self, name : String) -> @llvm.FunctionValue? {
  match self.llvm_funcs.get(name) {
    Some(func) => Some(func)
    None => None
  }
}

///|
traitalias @llvm.Value

///|
priv struct Env {
  gen : CodeGen
  parser_func : @parser.Function
  llvm_func : @llvm.FunctionValue
  parent : Env?
  symbols : Map[String, &Value]
}

///|
fn Env::new(
  gen : CodeGen,
  parser_func : @parser.Function,
  llvm_func : @llvm.FunctionValue
) -> Env {
  Env::{ gen, parser_func, llvm_func, parent: None, symbols: Map::new() }
}

///|
fn Env::subenv(self : Self) -> Env {
  Env::{
    gen: self.gen,
    parser_func: self.parser_func,
    llvm_func: self.llvm_func,
    parent: Some(self),
    symbols: Map::new(),
  }
}

///|
fn Env::get_symbol(self : Self, name : String) -> &Value? {
  match self.symbols.get(name) {
    Some(value) => Some(value)
    None =>
      match self.parent {
        Some(parent_env) => parent_env.get_symbol(name)
        None => None
      }
  }
}

///|
pub fn CodeGen::emitProg(self : Self) -> Unit raise Error {
  // collect struct definitions and functions types
  self.parser_prog.structs
  .values()
  .each(struct_def => self.emitStructDef(struct_def))
  let llvm_funcs = Map::new()
  self.parser_prog.functions.each((name, func) => llvm_funcs.set(
    name,
    self.genFuncVal(func),
  ))
  self.parser_prog.externs.each((name, func) => llvm_funcs.set(
    name,
    self.genFuncVal(func),
  ))
  for func_name in self.parser_prog.functions.keys() {
    let parser_func = match self.parser_prog.functions.get(func_name) {
      Some(func) => func
      None => raise CodeGenError("Error: Function not found: \{func_name}")
    }
    let llvm_func = match llvm_funcs.get(func_name) {
      Some(func) => func
      None => raise CodeGenError("Error: LLVM function not found: \{func_name}")
    }
    let env = Env::new(self, parser_func, llvm_func)
    env.emitFunc()
  }
}

///|
fn CodeGen::emitStructDef(
  self : Self,
  struct_def : @parser.StructDef
) -> Unit raise {
  // 将结构体字段类型转换为LLVM类型
  let field_types = struct_def.fields.map(field => {
    let (_, ty) = field
    self.convert_parser_type_to_llvm_basic_type(ty)
  })

  // 创建LLVM结构体类型并自动注册到context中
  let _ = self.llvm_ctx.struct_type(field_types)

}

///|
fn CodeGen::genFuncVal(
  self : Self,
  func : @parser.Function
) -> @llvm.FunctionValue raise {
  let param_tys = func.params.map(param_ty => {
    let (_, ty) = param_ty
    self.convert_parser_type_to_llvm_basic_type(ty)
  })
  let ret_ty = self.convert_parser_type_to_llvm_type(func.ret_ty)
  let func_ty = ret_ty.fn_type(param_tys)
  self.llvm_prog.add_function(func.name, func_ty)
}

///|
fn Env::emitFunc(self : Self) -> Unit raise {
  let llvm_ctx = self.gen.llvm_ctx
  let llvm_func = self.llvm_func
  let parser_func = self.parser_func
  let builder = self.gen.builder
  let entry = llvm_ctx.append_basic_block(llvm_func, name="entry")
  builder.position_at_end(entry)
  parser_func.params.eachi((i, name_ty) => {
    let (name, ty) = name_ty
    let param_val = llvm_func.get_nth_param(i.reinterpret_as_uint()).unwrap()
    let ty = self.gen.convert_parser_type_to_llvm_basic_type(ty)
    let alloca = builder.build_alloca(ty)
    let _ = builder.build_store(alloca, param_val)
    self.symbols.set(name, alloca)
  })
  parser_func.body.each(stmt => self.emitStmt(stmt))
}

///|
fn Env::emitStmt(self : Self, stmt : @parser.Stmt) -> Unit raise {
  match stmt {
    Let(name, ty, None) => {
      let data_ty = self.gen.convert_parser_type_to_llvm_basic_type(ty)
      let alloca = self.gen.builder.build_alloca(data_ty)
      self.symbols.set(name, alloca)
    }
    Let(name, ty, Some(expr)) => {
      let data_ty = self.gen.convert_parser_type_to_llvm_basic_type(ty)
      let alloca = self.gen.builder.build_alloca(data_ty)
      self.symbols.set(name, alloca)
      let value = self.emitExpr(expr)
      let _ = self.gen.builder.build_store(alloca, value)

    }
    Assign(left_val, expr) => {
      let ptr = self.emitLeftValue(left_val).into_pointer_value()
      let value = self.emitExpr(expr)
      let _ = self.gen.builder.build_store(ptr, value)

    }
    If(cond, then_stmts, []) => {
      let cond_val = self.emitExpr(cond).into_int_value()
      let then_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      let merge_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      let _ = self.gen.builder.build_conditional_branch(
        cond_val, then_block, merge_block,
      )
      self.gen.builder.position_at_end(then_block)
      let then_env = self.subenv()
      then_stmts.each(s => then_env.emitStmt(s))
      let _ = self.gen.builder.build_unconditional_branch(merge_block)
      self.gen.builder.position_at_end(merge_block)
    }
    If(cond, then_stmts, else_stmts) => {
      let cond_val = self.emitExpr(cond).into_int_value()
      let then_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      let else_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      let merge_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      //let _ = self.gen.builder.createCondBr(cond_val, then_block, else_block)
      let _ = self.gen.builder.build_conditional_branch(
        cond_val, then_block, else_block,
      )
      self.gen.builder.position_at_end(then_block)
      let then_env = self.subenv()
      then_stmts.each(s => then_env.emitStmt(s))
      let _ = self.gen.builder.build_unconditional_branch(merge_block)
      self.gen.builder.position_at_end(else_block)
      let else_env = self.subenv()
      else_stmts.each(s => else_env.emitStmt(s))
      let _ = self.gen.builder.build_unconditional_branch(merge_block)
      self.gen.builder.position_at_end(merge_block)
    }
    While(cond, stmts) => {
      let cond_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      let body_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      let merge_block = self.gen.llvm_ctx.append_basic_block(self.llvm_func)
      let _ = self.gen.builder.build_unconditional_branch(cond_block)
      self.gen.builder.position_at_end(cond_block)
      let cond_val = self.emitExpr(cond).into_int_value()
      let _ = self.gen.builder.build_conditional_branch(
        cond_val, body_block, merge_block,
      )
      self.gen.builder.position_at_end(body_block)
      let body_env = self.subenv()
      stmts.each(s => body_env.emitStmt(s))
      let _ = self.gen.builder.build_unconditional_branch(cond_block)
      self.gen.builder.position_at_end(cond_block)
    }
    Return(None) => {
      let _ = self.gen.builder.build_return_void()

    }
    Return(Some(expr)) => {
      let value = self.emitExpr(expr)
      let _ = self.gen.builder.build_return(value)

    }
    Expr(expr) => {
      let _ = self.emitExpr(expr)

    }
  }
  //_ => {
  //  raise CodeGenError("Error: Unsupported statement: \{stmt}");
  //}
}

///|
fn Env::emitLeftValue(
  self : Self,
  left_val : @parser.LeftValue
) -> &Value raise {
  match left_val {
    Var(name, ..) => {
      let addr = match self.get_symbol(name) {
        Some(value) => value
        None => raise CodeGenError("Error: Variable not found: \{name}")
      }
      addr
    }
    ArrayGet(array_val, index_expr, ty=Some(ele_ty)) => {
      let array_ptr = self.emitLeftValue(array_val).into_pointer_value()
      let index_val = self.emitExpr(index_expr).into_int_value()
      let ele_ty = self.gen.convert_parser_type_to_llvm_basic_type(ele_ty)
      let element_ptr = self.gen.builder.build_gep(ele_ty, array_ptr, [
        index_val,
      ])
      element_ptr
    }
    StructAccess(struct_val, field_name, ..) => {
      let struct_ptr = self.emitLeftValue(struct_val).into_pointer_value()

      // 需要根据左值的类型信息来获取结构体定义
      let struct_type_name = match struct_val {
        Var(_, ty=Some(Struct(sname))) => sname
        StructAccess(_, _, ty=Some(Struct(sname))) => sname
        ArrayGet(_, _, ty=Some(Struct(sname))) => sname
        _ =>
          raise CodeGenError(
            "Error: Cannot determine struct type for field access",
          )
      }
      let struct_def = match
        self.gen.parser_prog.structs.get(struct_type_name) {
        Some(def) => def
        None =>
          raise CodeGenError(
            "Error: Struct definition not found: \{struct_type_name}",
          )
      }
      //.or_else(() => {
      //  raise CodeGenError("Error: Struct definition not found: \{struct_type_name}")
      //})
      let field_index = match struct_def.get_field_index(field_name) {
        Some(index) => index
        None =>
          raise CodeGenError(
            "Error: Field \{field_name} not found in struct \{struct_type_name}",
          )
      }

      // 获取LLVM结构体类型
      let llvm_struct_type = match self.gen.struct_tys.get(struct_type_name) {
        Some(struct_ty) => struct_ty
        None =>
          raise CodeGenError(
            "Error: LLVM struct type not found: \{struct_type_name}",
          )
      }

      // 创建GEP指令：第一个索引0是解引用指针，第二个索引是字段索引
      let zero = self.gen.llvm_ctx.i32_type().const_zero()
      //let field_idx = self.gen.llvm_ctx.getConstInt32(field_index)
      let field_idx = self.gen.llvm_ctx
        .i32_type()
        .const_int(field_index.to_uint64())
      let field_ptr = self.gen.builder.build_gep(llvm_struct_type, struct_ptr, [
        zero, field_idx,
      ])
      field_ptr
    }
    _ => raise CodeGenError("Error: Unsupported left value: \{left_val}")
  }
}

///|
fn Env::emitExpr(self : Self, expr : @parser.Expr) -> &@llvm.BasicValue raise {
  match expr {
    Apply(apply_expr, ..) => self.emitApplyExpr(apply_expr).into_basic_value()
    Neg(expr, ty=Some(Int | Int64 as ty)) => {
      let value = self.emitExpr(expr).into_int_value()
      //let zero = self.gen.llvm_ctx.getConstInt32(0) as &Value;
      let zero = match ty {
        Int => self.gen.llvm_ctx.i32_type().const_zero()
        _ => self.gen.llvm_ctx.i64_type().const_zero()
      }
      self.gen.builder.build_int_sub(zero, value)
    }
    Neg(expr, ty=Some(_)) => {
      let value = self.emitExpr(expr).into_float_value()
      self.gen.builder.build_float_neg(value)
    }
    Neg(_, ..) => raise CodeGenError("Error: Unsupported negation type")
    Add(lhs, rhs, ty=Some(Int | Int64 | UInt | UInt64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_int_add(lhs_val, rhs_val)
    }
    Add(lhs, rhs, ty=Some(Float | Double)) => {
      let lhs_val = self.emitExpr(lhs).into_float_value()
      let rhs_val = self.emitExpr(rhs).into_float_value()
      self.gen.builder.build_float_add(lhs_val, rhs_val)
    }
    Add(_, ..) => raise CodeGenError("Error: Unsupported addition type")
    Sub(lhs, rhs, ty=Some(Int | Int64 | UInt | UInt64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_int_sub(lhs_val, rhs_val)
    }
    Sub(lhs, rhs, ty=Some(Float | Double)) => {
      let lhs_val = self.emitExpr(lhs).into_float_value()
      let rhs_val = self.emitExpr(rhs).into_float_value()
      self.gen.builder.build_float_sub(lhs_val, rhs_val)
    }
    Sub(_, ..) => raise CodeGenError("Error: Unsupported subtraction type")
    Mul(lhs, rhs, ty=Some(Int | Int64 | UInt | UInt64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_int_mul(lhs_val, rhs_val)
    }
    Mul(lhs, rhs, ty=Some(Float | Double)) => {
      let lhs_val = self.emitExpr(lhs).into_float_value()
      let rhs_val = self.emitExpr(rhs).into_float_value()
      self.gen.builder.build_float_mul(lhs_val, rhs_val)
    }
    Mul(_, ..) => raise CodeGenError("Error: Unsupported multiplication type")
    Div(lhs, rhs, ty=Some(Int | Int64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_int_signed_div(lhs_val, rhs_val)
    }
    Div(lhs, rhs, ty=Some(UInt | UInt64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_int_unsigned_div(lhs_val, rhs_val)
    }
    Div(lhs, rhs, ty=Some(Float | Double)) => {
      let lhs_val = self.emitExpr(lhs).into_float_value()
      let rhs_val = self.emitExpr(rhs).into_float_value()
      self.gen.builder.build_float_div(lhs_val, rhs_val)
    }
    Div(_, ..) => raise CodeGenError("Error: Unsupported division type")
    Rem(lhs, rhs, ty=Some(Int | Int64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_int_signed_rem(lhs_val, rhs_val)
    }
    Rem(lhs, rhs, ty=Some(UInt | UInt64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_int_unsigned_rem(lhs_val, rhs_val)
    }
    Rem(lhs, rhs, ty=Some(Float | Double)) => {
      let lhs_val = self.emitExpr(lhs).into_float_value()
      let rhs_val = self.emitExpr(rhs).into_float_value()
      self.gen.builder.build_float_rem(lhs_val, rhs_val)
    }
    Rem(_, ..) => raise CodeGenError("Error: Unsupported remainder type")
    Eq(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs)
      let rhs_val = self.emitExpr(rhs)
      match lhs_val.get_type().as_type_enum() {
        IntType(_) =>
          self.gen.builder.build_int_compare(
            EQ,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
          )
        FloatType(_) =>
          self.gen.builder.build_float_compare(
            OEQ,
            lhs_val.into_float_value(),
            rhs_val.into_float_value(),
          )
        _ => raise CodeGenError("Error: Unsupported type for equality")
      }
    }
    Ne(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs)
      let rhs_val = self.emitExpr(rhs)
      match lhs_val.get_type().as_type_enum() {
        IntType(_) =>
          self.gen.builder.build_int_compare(
            NE,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
          )
        FloatType(_) =>
          self.gen.builder.build_float_compare(
            ONE,
            lhs_val.into_float_value(),
            rhs_val.into_float_value(),
          )
        _ => raise CodeGenError("Error: Unsupported type for inequality")
      }
    }
    Lt(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs)
      let rhs_val = self.emitExpr(rhs)
      match lhs_val.get_type().as_type_enum() {
        IntType(_) =>
          self.gen.builder.build_int_compare(
            SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
          )
        FloatType(_) =>
          self.gen.builder.build_float_compare(
            OLT,
            lhs_val.into_float_value(),
            rhs_val.into_float_value(),
          )
        _ => raise CodeGenError("Error: Unsupported type for less than")
      }
    }
    Le(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs)
      let rhs_val = self.emitExpr(rhs)
      match lhs_val.get_type().as_type_enum() {
        IntType(_) =>
          self.gen.builder.build_int_compare(
            SLE,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
          )
        FloatType(_) =>
          self.gen.builder.build_float_compare(
            OLE,
            lhs_val.into_float_value(),
            rhs_val.into_float_value(),
          )
        _ =>
          raise CodeGenError("Error: Unsupported type for less than or equal")
      }
    }
    Gt(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs)
      let rhs_val = self.emitExpr(rhs)
      match lhs_val.get_type().as_type_enum() {
        IntType(_) =>
          self.gen.builder.build_int_compare(
            SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
          )
        FloatType(_) =>
          self.gen.builder.build_float_compare(
            OGT,
            lhs_val.into_float_value(),
            rhs_val.into_float_value(),
          )
        _ => raise CodeGenError("Error: Unsupported type for greater than")
      }
    }
    Ge(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs)
      let rhs_val = self.emitExpr(rhs)
      match lhs_val.get_type().as_type_enum() {
        IntType(_) =>
          self.gen.builder.build_int_compare(
            SGE,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
          )
        FloatType(_) =>
          self.gen.builder.build_float_compare(
            OGE,
            lhs_val.into_float_value(),
            rhs_val.into_float_value(),
          )
        _ =>
          raise CodeGenError(
            "Error: Unsupported type for greater than or equal",
          )
      }
    }
    And(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_and(lhs_val, rhs_val)
    }
    Or(lhs, rhs) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_or(lhs_val, rhs_val)
    }
    Shl(lhs, rhs, ..) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_left_shift(lhs_val, rhs_val)
    }
    Shr(lhs, rhs, ty=Some(UInt | UInt64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_right_shift(lhs_val, rhs_val, sign_extend=false)
    }
    Shr(lhs, rhs, ty=Some(Int | Int64)) => {
      let lhs_val = self.emitExpr(lhs).into_int_value()
      let rhs_val = self.emitExpr(rhs).into_int_value()
      self.gen.builder.build_right_shift(lhs_val, rhs_val, sign_extend=true)
    }
    Shr(_, ..) => raise CodeGenError("Error: Unsupported shift right type")
  }
}

///|
fn Env::emitApplyExpr(
  self : Self,
  apply_expr : @parser.ApplyExpr
) -> &@llvm.Value raise {
  match apply_expr {
    Atom(atom_expr, ..) => self.emitAtomExpr(atom_expr)
    ArrayGet(array_val, index_expr, ty=Some(ele_ty)) => {
      let array_ptr = self.emitApplyExpr(array_val).into_pointer_value()
      let index_val = self.emitExpr(index_expr).into_int_value()
      let ele_ty = self.gen.convert_parser_type_to_llvm_basic_type(ele_ty)
      let element_ptr = self.gen.builder.build_gep(ele_ty, array_ptr, [
        index_val,
      ])
      self.gen.builder.build_load(ele_ty, element_ptr)
    }
    StructAccess(struct_val, field_name, ..) => {
      let struct_ptr = self.emitApplyExpr(struct_val).into_pointer_value()

      // 需要根据Apply表达式的类型信息来获取结构体定义
      let struct_type_name = match struct_val {
        Atom(Var(_, ty=Some(Struct(sname))), ..) => sname
        StructAccess(_, _, ty=Some(Struct(sname))) => sname
        ArrayGet(_, _, ty=Some(Struct(sname))) => sname
        _ =>
          raise CodeGenError(
            "Error: Cannot determine struct type for field access",
          )
      }
      let struct_def = match
        self.gen.parser_prog.structs.get(struct_type_name) {
        Some(def) => def
        None =>
          raise CodeGenError(
            "Error: Struct definition not found: \{struct_type_name}",
          )
      }
      let field_index = match struct_def.get_field_index(field_name) {
        Some(index) => index
        None =>
          raise CodeGenError(
            "Error: Field \{field_name} not found in struct \{struct_type_name}",
          )
      }
      let field_type = match struct_def.get_field_type(field_name) {
        Some(ty) => ty
        None =>
          raise CodeGenError(
            "Error: Field type not found for \{field_name} in struct \{struct_type_name}",
          )
      }

      // 获取LLVM结构体类型
      let llvm_struct_type = match self.gen.struct_tys.get(struct_type_name) {
        Some(struct_ty) => struct_ty
        None =>
          raise CodeGenError(
            "Error: LLVM struct type not found: \{struct_type_name}",
          )
      }

      // 创建GEP指令获取字段指针
      let zero = self.gen.llvm_ctx.i32_type().const_zero()
      //let field_idx = self.gen.llvm_ctx.getConstInt32(field_index)
      let field_idx = self.gen.llvm_ctx
        .i32_type()
        .const_int(field_index.to_uint64())
      let field_ptr = self.gen.builder.build_gep(llvm_struct_type, struct_ptr, [
        zero, field_idx,
      ])

      // 加载字段值
      let field_llvm_type = self.gen.convert_parser_type_to_llvm_basic_type(
        field_type,
      )
      self.gen.builder.build_load(field_llvm_type, field_ptr)
    }
    Cast(left, cast_ty) => {
      let value = self.emitApplyExpr(left)
      let target_ty = self.gen.convert_parser_type_to_llvm_type(cast_ty)
      match (value.get_type().as_type_enum(), target_ty.as_type_enum()) {
        // Integer to float
        (IntType(_), FloatType(float_ty)) =>
          //self.gen.builder.createSIToFP(value, float_ty)
          self.gen.builder.build_cast(SIToFP, value.into_int_value(), float_ty)

        // Float to integer
        (FloatType(_), IntType(int32_ty)) =>
          self.gen.builder.build_cast(
            FPToSI,
            value.into_float_value(),
            int32_ty,
          )

        // Float to float
        (FloatType(from_ty), FloatType(to_ty)) => {
          let from_ty_size = from_ty
            .size_of()
            .unwrap()
            .get_sign_extended_constant()
            .unwrap()
          let to_ty_size = to_ty
            .size_of()
            .unwrap()
            .get_sign_extended_constant()
            .unwrap()
          if from_ty_size < to_ty_size {
            self.gen.builder.build_cast(
              FPTrunc,
              value.into_float_value(),
              to_ty,
            )
          } else if from_ty_size > to_ty_size {
            self.gen.builder.build_cast(FPExt, value.into_float_value(), to_ty)
          } else {
            value.into_float_value()
          }
        }
        (IntType(from_ty), IntType(to_ty)) => {
          let from_ty_size = from_ty
            .size_of()
            .unwrap()
            .get_sign_extended_constant()
            .unwrap()
          let to_ty_size = to_ty
            .size_of()
            .unwrap()
            .get_sign_extended_constant()
            .unwrap()
          if from_ty_size < to_ty_size {
            self.gen.builder.build_cast(SExt, value.into_int_value(), to_ty)
          } else if from_ty_size > to_ty_size {
            self.gen.builder.build_cast(Trunc, value.into_int_value(), to_ty)
          } else {
            value.into_int_value()
          }
        }

        // Same type
        (from_ty, to_ty) if from_ty.as_type_class() == to_ty.as_type_class() =>
          value.into_basic_value()
        _ =>
          raise CodeGenError(
            "Error: Unsupported cast from \{value.get_type()} to \{target_ty}",
          )
      }
    }
    _ =>
      raise CodeGenError("Error: Unsupported apply expression: \{apply_expr}")
  }
}

///|
fn Env::emitAtomExpr(
  self : Self,
  atomExpr : @parser.AtomExpr
) -> &@llvm.Value raise {
  let ctx = self.gen.llvm_ctx
  match atomExpr {
    Bool(true) => {
      ctx.i32_type().const_int(1) as &@llvm.Value
    }
    Bool(false) => ctx.i32_type().const_zero()
    Int(v) => ctx.i32_type().const_int(v.to_uint64())
    UInt(v) => ctx.i32_type().const_int(v.to_uint64())
    Int64(v) => ctx.i32_type().const_int(v.reinterpret_as_uint64())
    UInt64(v) => ctx.i32_type().const_int(v)
    Float(v) => ctx.f32_type().const_float(v.to_double())
    Double(v) => ctx.f32_type().const_float(v)
    Var(name, ty=Some(ty)) => {
      let addr = match self.get_symbol(name) {
        Some(value) => value.into_pointer_value()
        None => raise CodeGenError("Error: Variable not found: \{name}")
      }
      let ty = self.gen.convert_parser_type_to_llvm_basic_type(ty)
      self.gen.builder.build_load(ty, addr)
    }
    // type must be a pointer type
    Ref(name, ..) =>
      match self.symbols.get(name) {
        Some(value) => value.into_pointer_value()
        None => raise CodeGenError("Error: Reference not found: \{name}")
      }
    //.or_else(() => {
    //  raise CodeGenError("Error: Reference not found: \{name}")
    //})
    TypeSizeof(ty) =>
      match self.gen.convert_parser_type_to_llvm_type(ty).size_of() {
        Some(size) => size
        None => raise CodeGenError("Error: get Type size compute failed: \{ty}")
      }
    ExprSizeof(_) => raise CodeGenError("Error: ExprSizeof not implemented yet")
    Array(exprs, ty=Some(Array(ele_ty))) => {
      let ele_ty = self.gen.convert_parser_type_to_llvm_basic_type(ele_ty)
      let array_size = exprs.length().reinterpret_as_uint()
      let arr_ty = ele_ty.array_type(array_size)
      let array_ptr = self.gen.builder.build_alloca(arr_ty)
      exprs.eachi((i, expr) => {
        let value = self.emitExpr(expr)
        let index = self.gen.llvm_ctx.i32_type().const_int(i.to_uint64())
        let element_ptr = self.gen.builder.build_gep(ele_ty, array_ptr, [index])
        let _ = self.gen.builder.build_store(element_ptr, value)

      })
      array_ptr
    }
    Paren(expr, ..) => self.emitExpr(expr)
    Call(func_name, args, ..) => {
      let llvm_func = match self.gen.getFunction(func_name) {
        Some(func) => func
        None =>
          raise CodeGenError("Error: LLVM function not found: \{func_name}")
      }
      let args_vals = args.map(arg => self.emitExpr(arg))
      self.gen.builder.build_call(llvm_func, args_vals)
    }
    _ => raise CodeGenError("Error: Unsupported atom expression: \{atomExpr}")
  }
}
