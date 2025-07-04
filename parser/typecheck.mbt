///|
pub struct Env[K, V] {
  parent : Env[K, V]?
  data : Map[K, V]
}

///|
pub fn[K, V] Env::new(parent~ : Env[K, V]? = None) -> Env[K, V] {
  Env::{ parent, data: Map::new() }
}

///|
pub fn[K : Eq + Hash, V] Env::get(self : Self[K, V], key : K) -> V? {
  match self.data.get(key) {
    Some(value) => Some(value)
    None =>
      match self.parent {
        Some(parent_env) => parent_env.get(key)
        None => None
      }
  }
}

///|
pub fn[K : Eq + Hash + Show, V] Env::get_or_err(
  self : Self[K, V],
  key : K
) -> V {
  match self.data.get(key) {
    Some(value) => value
    None =>
      match self.parent {
        Some(parent_env) => parent_env.get_or_err(key)
        None => {
          println("Error: Key not found: \{key}")
          panic()
        }
      }
  }
}

///|
pub fn[K : Eq + Hash + Show, V] Env::set(
  self : Self[K, V],
  key : K,
  value : V
) -> Unit {
  if self.data.contains(key) {
    println("Error: Duplicate key: \{key}")
    panic()
  }
  self.data.set(key, value)
}

///|
pub struct TypeCheck {
  local_env : Env[String, Type]
  func : Function
  prog : Program
}

///|
pub fn TypeCheck::sub_env(self : Self) -> TypeCheck {
  TypeCheck::{
    local_env: Env::new(parent=Some(self.local_env)),
    func: self.func,
    prog: self.prog,
  }
}

///|
pub fn TypeCheck::check_prog(prog : Program) -> Unit {
  for func_entry in prog.functions {
    let (_, func) = func_entry
    TypeCheck::{ local_env: Env::new(), func, prog }.check_function(func)
  }
}

///|
pub fn TypeCheck::check_function(self : Self, func : Function) -> Unit {
  for param in func.params {
    let (name, ty) = param
    self.local_env.set(name, ty)
  }
  func.body.each(stmt => self.check_stmt(stmt))
}

///|
pub fn TypeCheck::check_stmt(self : Self, stmt : Stmt) -> Unit {
  match stmt {
    Let(var_name, ty, None) => {
      // let arr: Array[Int]; is not allowed
      guard not(ty is Array(_)) else {
        println("Error: Array type must be initialized with values")
        panic()
      }
      self.local_env.set(var_name, ty)
    }
    Let(var_name, ty, Some(expr)) => {
      let expr_ty = self.check_expr(expr)
      guard expr_ty == ty else {
        println("Let Statement Type Mismatch: Expected \{ty}, got \{expr_ty}")
        panic()
      }
      self.local_env.set(var_name, ty)
    }
    Assign(left_value, expr) => {
      let left_ty = self.check_left_value(left_value)
      let expr_ty = self.check_expr(expr)
      guard left_ty == expr_ty else {
        println("Assignment Type Mismatch: Expected \{left_ty}, got \{expr_ty}")
        panic()
      }
    }
    If(cond, then_block, else_block) => {
      let cond_ty = self.check_expr(cond)
      guard cond_ty is Bool else {
        println("If Condition Type Mismatch: Expected Bool, got \{cond_ty}")
        panic()
      }
      then_block.each(stmt => self.sub_env().check_stmt(stmt))
      else_block.each(stmt => self.sub_env().check_stmt(stmt))
    }
    While(cond, body) => {
      let cond_ty = self.check_expr(cond)
      guard cond_ty is Bool else {
        println("While Condition Type Mismatch: Expected Bool, got \{cond_ty}")
        panic()
      }
      body.each(stmt => self.sub_env().check_stmt(stmt))
    }
    Return(Some(expr)) => {
      let expr_ty = self.check_expr(expr)
      guard expr_ty == self.func.ret_ty else {
        println(
          "Return Type Mismatch: Expected \{self.func.ret_ty}, got \{expr_ty}",
        )
        panic()
      }
    }
    Return(None) => {
      guard self.func.ret_ty is Unit else {
        println("Return Type Mismatch: Expected Void, got \{self.func.ret_ty}")
        panic()
      }
    }
    Expr(expr) => {
      let _ = self.check_expr(expr)

    }
  }
}

///|
pub fn TypeCheck::check_expr(self : Self, expr : Expr) -> Type {
  match expr {
    Apply(apply_expr, ..) as a => {
      let ty = self.check_apply(apply_expr)
      a.ty = Some(ty)
      ty
    }
    Neg(expr, ..) as neg_expr => {
      let expr_ty = self.check_expr(expr)
      neg_expr.ty = Some(expr_ty)
      expr_ty
    }
    Add(lhs, rhs, ..) as add_expr => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Add Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      add_expr.ty = Some(lhs_ty)
      lhs_ty
    }
    Sub(lhs, rhs, ..) as sub_expr => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Sub Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      sub_expr.ty = Some(lhs_ty)
      lhs_ty
    }
    Mul(lhs, rhs, ..) as mul_expr => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Mul Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      mul_expr.ty = Some(lhs_ty)
      lhs_ty
    }
    Div(lhs, rhs, ..) as div_expr => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Div Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      div_expr.ty = Some(lhs_ty)
      lhs_ty
    }
    Rem(lhs, rhs, ..) as rem_expr => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Rem Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      rem_expr.ty = Some(lhs_ty)
      lhs_ty
    }
    Eq(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Eq Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      Bool
    }
    Ne(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Neq Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      Bool
    }
    Lt(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Lt Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      Bool
    }
    Le(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Le Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      Bool
    }
    Gt(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Gt Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      Bool
    }
    Ge(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Ge Type Mismatch: Expected \{lhs_ty}, got \{rhs_ty}")
        panic()
      }
      Bool
    }
    And(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == Bool && rhs_ty == Bool else {
        println("And Type Mismatch: Expected Bool, got \{lhs_ty} and \{rhs_ty}")
        panic()
      }
      Bool
    }
    Or(lhs, rhs) => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == Bool && rhs_ty == Bool else {
        println("Or Type Mismatch: Expected Bool, got \{lhs_ty} and \{rhs_ty}")
        panic()
      }
      Bool
    }
    Shl(lhs, rhs, ..) as shl_expr => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Shl Type Mismatch: Expected Int, got \{lhs_ty} and \{rhs_ty}")
        panic()
      }
      guard lhs_ty is (Int | UInt | Int64 | UInt64) else {
        println("Shl Type Mismatch: Expected Int or UInt, got \{lhs_ty}")
        panic()
      }
      shl_expr.ty = Some(lhs_ty)
      Int
    }
    Shr(lhs, rhs, ..) as shr_expr => {
      let lhs_ty = self.check_expr(lhs)
      let rhs_ty = self.check_expr(rhs)
      guard lhs_ty == rhs_ty else {
        println("Shr Type Mismatch: Expected Int, got \{lhs_ty} and \{rhs_ty}")
        panic()
      }
      guard lhs_ty is (Int | UInt | Int64 | UInt64) else {
        println("Shr Type Mismatch: Expected Int or UInt, got \{lhs_ty}")
        panic()
      }
      shr_expr.ty = Some(lhs_ty)
      Int
    }
  }
}

///|
pub fn TypeCheck::check_left_value(self : Self, left_value : LeftValue) -> Type {
  match left_value {
    Var(name, ..) as var_left => {
      let ty = self.local_env.get_or_err(name)
      var_left.ty = Some(ty)
      ty
    }
    ArrayGet(left, index, ..) as array_get => {
      let left_ty = self.check_left_value(left)
      let index_ty = self.check_expr(index)
      guard index_ty is Int else {
        println("ArrayGet Index Type Mismatch: Expected Int, got \{index_ty}")
        panic()
      }
      guard left_ty is Array(arr_ty) else {
        println(
          "ArrayGet Left Value Type Mismatch: Expected ArrayType, got \{left_ty}",
        )
        panic()
      }
      array_get.ty = Some(arr_ty)
      arr_ty
    }
    StructAccess(left, field_name, ..) as struct_access => {
      let left_ty = self.check_left_value(left)
      guard left_ty is Struct(struct_ty) else {
        println(
          "StructAccess Left Value Type Mismatch: Expected StructType, got \{left_ty}",
        )
        panic()
      }
      let struct_def = self.prog.structs
        .get(struct_ty)
        .or_else(() => {
          println("Error: Struct \{struct_ty} not found in program")
          panic()
        })
      let ty = struct_def
        .get_field_type(field_name)
        .or_else(() => {
          println("Error: Field \{field_name} not found in struct \{struct_ty}")
          panic()
        })
      struct_access.ty = Some(ty)
      ty
    }
  }
}

///|
pub fn TypeCheck::check_apply(self : Self, apply_expr : ApplyExpr) -> Type {
  match apply_expr {
    Atom(atom_expr, ..) as a => {
      let ty = self.check_atom_expr(atom_expr)
      a.ty = Some(ty)
      ty
    }
    ArrayGet(left, index, ..) as array_get => {
      let left_ty = self.check_apply(left)
      let index_ty = self.check_expr(index)
      guard index_ty is Int else {
        println("ArrayGet Index Type Mismatch: Expected Int, got \{index_ty}")
        panic()
      }
      guard left_ty is Array(arr_ty) else {
        println(
          "ArrayGet Left Value Type Mismatch: Expected ArrayType, got \{left_ty}",
        )
        panic()
      }
      array_get.ty = Some(arr_ty)
      arr_ty
    }
    StructAccess(left, field_name, ..) as struct_access => {
      let left_ty = self.check_apply(left)
      guard left_ty is Struct(struct_ty) else {
        println(
          "StructAccess Left Value Type Mismatch: Expected StructType, got \{left_ty}",
        )
        panic()
      }
      let struct_def = self.prog.structs
        .get(struct_ty)
        .or_else(() => {
          println("Error: Struct \{struct_ty} not found in program")
          panic()
        })
      let ty = struct_def
        .get_field_type(field_name)
        .or_else(() => {
          println("Error: Field \{field_name} not found in struct \{struct_ty}")
          panic()
        })
      struct_access.ty = Some(ty)
      ty
    }
    Cast(left, cast_ty) => {
      let _ = self.check_apply(left)
      cast_ty
    }
  }
}

///|
pub fn TypeCheck::check_atom_expr(self : Self, atom_expr : AtomExpr) -> Type {
  match atom_expr {
    Bool(_) => Bool
    Int(_) => Int
    Int64(_) => Int64
    UInt(_) => UInt
    UInt64(_) => UInt64
    Float(_) => Float
    Double(_) => Double
    Var(name, ..) as a => {
      let ty = self.local_env.get_or_err(name)
      a.ty = Some(ty)
      ty
    }
    Ref(name, ..) as a => {
      let ty = self.local_env.get_or_err(name) |> Ptr
      a.ty = Some(ty)
      ty
    }
    TypeSizeof(_) => Int
    ExprSizeof(_) => Int
    Array(exprs, ..) as array_expr => {
      let exprs_tys = exprs.map(self.check_expr(_))
      let last = exprs_tys
        .last()
        .or_else(() => {
          println("Error: Empty array in AtomExpr")
          panic()
        })
      guard exprs_tys.iter().all(ty => ty == last) else {
        println("Error: Array element types do not match: \{exprs_tys}")
        panic()
      }
      let ty = Type::Array(last)
      array_expr.ty = Some(ty)
      ty
    }
    Paren(expr, ..) as paren_expr => {
      let expr_ty = self.check_expr(expr)
      paren_expr.ty = Some(expr_ty)
      expr_ty
    }
    Call(func_name, args, ..) as call_expr => {
      //let func = self.prog.functions
      //  .get(func_name)
      //  .or_else(() => {
      //      println("Error: Function \{func_name} not found in program")
      //      panic()
      //    }
      //  )
      let func = match self.prog.functions.get(func_name) {
        Some(f) => f
        None =>
          match self.prog.externs.get(func_name) {
            Some(f) => f
            None => {
              println("Error: Function \{func_name} not found in program")
              panic()
            }
          }
      }
      let args_tys = args.map(self.check_expr(_))
      let param_tys = func.params.map(param => param.1)
      guard args_tys == param_tys else {
        println(
          "Error: Function \{func_name} argument types do not match. Expected \{param_tys}, got \{args_tys}",
        )
        panic()
      }
      call_expr.ty = Some(func.ret_ty)
      func.ret_ty
    }
  }
}
