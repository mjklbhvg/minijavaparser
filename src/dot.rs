use crate::types::*;

struct Node<'a> {
    name: Label<'a>,
    children: Vec<Node<'a>>,
}

enum Label<'a> {
    Str(&'a str),
    Num(u64),
}

macro_rules! node {
    ($name: expr) => {
        Node {
            name: Label::Str($name),
            children: vec![],
        }
    };
}

trait GrowSyntaxTree {
    fn to_tree(&self) -> Node;
}

impl GrowSyntaxTree for Program {
    fn to_tree(&self) -> Node {
        let mut root = node!("program");

        for decl in &self.decls {
            root.children.push(decl.to_tree());
        }
        for stmt in &self.stmts {
            root.children.push(stmt.to_tree());
        }
        return root;
    }
}

impl GrowSyntaxTree for Decl {
    fn to_tree(&self) -> Node {
        let mut root = Node {
            name: Label::Str("decl"),
            children: vec![Node {
                name: Label::Str("type"),
                children: vec![node!("int")],
            }],
        };

        let mut i = 0;
        for ident in &self.idents {
            i += 1;
            root.children.push(Node {
                name: Label::Str("name"),
                children: vec![node!(ident)],
            });

            if i != self.idents.len() {
                root.children.push(node!(","));
            }
        }
        root.children.push(node!(";"));
        return root;
    }
}

impl GrowSyntaxTree for Stmt {
    fn to_tree(&self) -> Node {
        let mut root = node!("stmt");
        match self {
            Stmt::Nop => {
                root.children.push(node!(";"));
            }
            Stmt::Block(stmts) => {
                root.children.push(node!("{"));
                for stmt in stmts {
                    root.children.push(stmt.to_tree());
                }
                root.children.push(node!("}"));
            }
            Stmt::Assignment { name, expr } => {
                root.children.push(Node {
                    name: Label::Str("name"),
                    children: vec![node!(name)],
                });
                root.children.push(node!("="));
                root.children.push(expr.to_tree());
                root.children.push(node!(";"));
            }
            Stmt::IoRead(name) => {
                root.children.push(Node {
                    name: Label::Str("name"),
                    children: vec![node!(name)],
                });
                root.children.push(node!("="));
                root.children.push(node!("read()"));
                root.children.push(node!(";"));
            }
            Stmt::IoWrite(expr) => {
                root.children.push(node!("write("));
                root.children.push(expr.to_tree());
                root.children.push(node!(")"));
                root.children.push(node!(";"));
            }
            Stmt::If { cond, stmt } => {
                root.children.push(node!("if"));
                root.children.push(node!("("));
                root.children.push(cond.to_tree());
                root.children.push(node!(")"));
                root.children.push(stmt.to_tree());
            }
            Stmt::IfElse {
                cond,
                stmt,
                else_stmt,
            } => {
                root.children.push(node!("if"));
                root.children.push(node!("("));
                root.children.push(cond.to_tree());
                root.children.push(node!(")"));
                root.children.push(stmt.to_tree());
                root.children.push(node!("else"));
                root.children.push(else_stmt.to_tree());
            }
            Stmt::While { cond, stmt } => {
                root.children.push(node!("while"));
                root.children.push(node!("("));
                root.children.push(cond.to_tree());
                root.children.push(node!(")"));
                root.children.push(stmt.to_tree());
            }
        }
        return root;
    }
}

impl GrowSyntaxTree for Expr {
    fn to_tree(&self) -> Node {
        let mut root = node!("expr");
        match self {
            Expr::Number(n) => {
                let mut num = node!("number");
                num.children.push(Node {
                    name: Label::Num(*n),
                    children: vec![],
                });
                root.children.push(num);
            }
            Expr::Name(n) => {
                root.children.push(Node {
                    name: Label::Str("name"),
                    children: vec![node!(n)],
                });
            }
            Expr::Expr(expr) => {
                root.children.push(node!("("));
                root.children.push(expr.to_tree());
                root.children.push(node!(")"));
            }
            Expr::Unop { op, expr } => {
                root.children.push(op.to_tree());
                root.children.push(expr.to_tree());
            }
            Expr::Binop { op, left, right } => {
                root.children.push(left.to_tree());
                root.children.push(op.to_tree());
                root.children.push(right.to_tree());
            }
        };
        return root;
    }
}

impl GrowSyntaxTree for Cond {
    fn to_tree(&self) -> Node {
        let mut root = node!("cond");
        match self {
            Cond::True => root.children.push(node!("true")),
            Cond::False => root.children.push(node!("false")),
            Cond::Nop(cond) => {
                root.children.push(node!("("));
                root.children.push(cond.to_tree());
                root.children.push(node!(")"));
            }
            Cond::Comp { op, left, right } => {
                root.children.push(left.to_tree());
                root.children.push(op.to_tree());
                root.children.push(right.to_tree());
            }
            Cond::Bunop { op, cond } => {
                root.children.push(op.to_tree());
                root.children.push(cond.to_tree());
            }
            Cond::BBinop { op, left, right } => {
                root.children.push(left.to_tree());
                root.children.push(op.to_tree());
                root.children.push(right.to_tree());
            }
        };
        return root;
    }
}

impl GrowSyntaxTree for Bunop {
    fn to_tree(&self) -> Node {
        Node {
            name: Label::Str("bunop"),
            children: vec![node!("!")], // there is only one bunop
        }
    }
}

impl GrowSyntaxTree for BBinop {
    fn to_tree(&self) -> Node {
        let mut root = node!("bbinop");
        match self {
            BBinop::And => root.children.push(node!("&&")),
            BBinop::Or => root.children.push(node!("||")),
        }
        return root;
    }
}

impl GrowSyntaxTree for Unop {
    fn to_tree(&self) -> Node {
        Node {
            name: Label::Str("unop"),
            children: vec![node!("-")], // there is only one unop
        }
    }
}

impl GrowSyntaxTree for Binop {
    fn to_tree(&self) -> Node {
        let mut root = node!("binop");
        match self {
            Binop::Minus => root.children.push(node!("-")),
            Binop::Plus => root.children.push(node!("+")),
            Binop::Multilply => root.children.push(node!("*")),
            Binop::Divide => root.children.push(node!("/")),
            Binop::Modulo => root.children.push(node!("%")),
        }
        return root;
    }
}

impl GrowSyntaxTree for Compop {
    fn to_tree(&self) -> Node {
        let mut root = node!("compop");
        match self {
            Compop::Eq => root.children.push(node!("==")),
            Compop::Neq => root.children.push(node!("!=")),
            Compop::Lte => root.children.push(node!("<=")),
            Compop::Gte => root.children.push(node!(">=")),
            Compop::Lt => root.children.push(node!("<")),
            Compop::Gt => root.children.push(node!(">")),
        }
        return root;
    }
}

pub fn dump(prog: &Program) {
    println!("digraph Program{} {{", (prog as *const Program) as usize);
    dump_rec(&prog.to_tree());
    println!("}}");
}

fn dump_rec(node: &Node) {
    print!("\tnode{} [label=\"", (node as *const Node) as usize);
    match node.name {
        Label::Str(s) => print!("{}", s),
        Label::Num(n) => print!("{}", n),
    }
    println!("\"];");
    for child in &node.children {
        dump_rec(child);
        println!(
            "\tnode{} -> node{};",
            (node as *const Node) as usize,
            (child as *const Node) as usize
        );
    }
}
