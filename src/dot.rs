use crate::types::*;

struct Node {
    name: String,
    children: Vec<Node>,
}

macro_rules! node {
    ($name: expr) => {
        Node {
            name: $name,
            children: vec![],
        }
    }
}

trait GrowSyntaxTree {
    fn to_tree(&self) -> Node;
}

impl GrowSyntaxTree for Program {
    fn to_tree(&self) -> Node {
        let mut root = node!("program".to_owned());

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
            name: "decl".to_owned(),
            children: vec![
                Node {
                    name: "type".to_owned(),
                    children: vec![node!("int".to_owned())],
                }
            ],
        };

        let mut i = 0;
        for ident in &self.idents {
            i += 1;
            root.children.push(Node {
                name: "name".to_owned(),
                children: vec![node!(ident.to_owned())],
            });

            if i != self.idents.len() {
                root.children.push(node!(",".to_owned()));
            }
        }
        root.children.push(node!(";".to_owned()));
        return root;
    }
}

impl GrowSyntaxTree for Stmt {
    fn to_tree(&self) -> Node {
        let mut root = node!("stmt".to_owned());
        match self {
            Stmt::Nop => { root.children.push(node!(";".to_owned())); },
            Stmt::Block(stmts) => {
                root.children.push(node!("{".to_owned()));
                for stmt in stmts {
                    root.children.push(stmt.to_tree());
                }
                root.children.push(node!("}".to_owned()));
            },
            Stmt::Assignment { name, expr } => {
                root.children.push(Node {
                    name: "name".to_owned(),
                    children: vec![node!(name.clone())]
                });
                root.children.push(node!("=".to_owned()));
                root.children.push(expr.to_tree());
                root.children.push(node!(";".to_owned()));
            },
            Stmt::IoRead(name) => {
                root.children.push(Node {
                    name: "name".to_owned(),
                    children: vec![node!(name.clone())]
                });
                root.children.push(node!("=".to_owned()));
                root.children.push(node!("read()".to_owned()));
                root.children.push(node!(";".to_owned()));
            },
            Stmt::IoWrite(expr) => {
                root.children.push(node!("write(".to_owned()));
                root.children.push(expr.to_tree());
                root.children.push(node!(")".to_owned()));
                root.children.push(node!(";".to_owned()));
            },
            Stmt::If { cond, stmt } => {
                root.children.push(node!("if".to_owned()));
                root.children.push(node!("(".to_owned()));
                root.children.push(cond.to_tree());
                root.children.push(node!(")".to_owned()));
                root.children.push(stmt.to_tree());
            },
            Stmt::IfElse { cond, stmt, else_stmt } => {
                root.children.push(node!("if".to_owned()));
                root.children.push(node!("(".to_owned()));
                root.children.push(cond.to_tree());
                root.children.push(node!(")".to_owned()));
                root.children.push(stmt.to_tree());
                root.children.push(node!("else".to_owned()));
                root.children.push(else_stmt.to_tree());
            },
            Stmt::While { cond, stmt } => {
                root.children.push(node!("while".to_owned()));
                root.children.push(node!("(".to_owned()));
                root.children.push(cond.to_tree());
                root.children.push(node!(")".to_owned()));
                root.children.push(stmt.to_tree());
            },
        }
        return root;
    }
}

impl GrowSyntaxTree for Expr {
    fn to_tree(&self) -> Node {
        let mut root = node!("expr".to_owned());
        match self {
            Expr::Number(n) => {
                root.children.push(Node {
                    name: "number".to_owned(),
                    children: vec![node!(n.to_string())],
                });
            },
            Expr::Name(n) => {
                root.children.push(Node {
                    name: "name".to_owned(),
                    children: vec![node!(n.clone())],
                });
            },
            Expr::Expr(expr) => {
                root.children.push(node!("(".to_owned()));
                root.children.push(expr.to_tree());
                root.children.push(node!(")".to_owned()));
            },
            Expr::Unop { op, expr } => {
                root.children.push(op.to_tree());
                root.children.push(expr.to_tree());
            },
            Expr::Binop { op, left, right } => {
                root.children.push(left.to_tree());
                root.children.push(op.to_tree());
                root.children.push(right.to_tree());
            },
        };
        return root;
    }
}

impl GrowSyntaxTree for Cond {
    fn to_tree(&self) -> Node {
        let mut root = node!("cond".to_owned());
        match self {
            Cond::True => { root.children.push(node!("true".to_owned())) },
            Cond::False => { root.children.push(node!("false".to_owned())) },
            Cond::Nop(cond) => {
                root.children.push(node!("(".to_owned()));
                root.children.push(cond.to_tree());
                root.children.push(node!(")".to_owned()));
            },
            Cond::Comp { op, left, right } => {
                root.children.push(left.to_tree());
                root.children.push(op.to_tree());
                root.children.push(right.to_tree());
            },
            Cond::Bunop { op, cond } => {
                root.children.push(op.to_tree());
                root.children.push(cond.to_tree());
            },
            Cond::BBinop { op, left, right } => {
                root.children.push(left.to_tree());
                root.children.push(op.to_tree());
                root.children.push(right.to_tree());
            },
        };
        return root;
    }
}

impl GrowSyntaxTree for Bunop {
    fn to_tree(&self) -> Node {
        Node {
            name: "bunop".to_owned(),
            children: vec![node!("!".to_owned())], // there is only one bunop
        }
    }
}

impl GrowSyntaxTree for BBinop {
    fn to_tree(&self) -> Node {
        let mut root = node!("bbinop".to_owned());
        match self {
            BBinop::And => { root.children.push(node!("&&".to_owned())) },
            BBinop::Or => { root.children.push(node!("||".to_owned())) },
        }
        return root;
    }
}

impl GrowSyntaxTree for Unop {
    fn to_tree(&self) -> Node {
        Node {
            name: "unop".to_owned(),
            children: vec![node!("-".to_owned())], // there is only one unop
        }
    }
}

impl GrowSyntaxTree for Binop {
    fn to_tree(&self) -> Node {
        let mut root = node!("binop".to_owned());
        match self {
            Binop::Minus => { root.children.push(node!("-".to_owned())) },
            Binop::Plus => { root.children.push(node!("+".to_owned())) },
            Binop::Multilply => { root.children.push(node!("*".to_owned())) },
            Binop::Divide => { root.children.push(node!("/".to_owned())) },
            Binop::Modulo => { root.children.push(node!("%".to_owned())) },
        }
        return root;
    }
}

impl GrowSyntaxTree for Compop {
    fn to_tree(&self) -> Node {
        let mut root = node!("compop".to_owned());
        match self {
            Compop::Eq => { root.children.push(node!("==".to_owned())) },
            Compop::Neq => { root.children.push(node!("!=".to_owned())) },
            Compop::Lte => { root.children.push(node!("<=".to_owned())) },
            Compop::Gte => { root.children.push(node!(">=".to_owned())) },
            Compop::Lt => { root.children.push(node!("<".to_owned())) },
            Compop::Gt => { root.children.push(node!(">".to_owned())) },
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
    println!("\tnode{} [label=\"{}\"];", (node as *const Node) as usize, node.name);
    for child in &node.children {
        dump_rec(child);
        println!(
            "\tnode{} -> node{};", (node as *const Node) as usize,
                                   (child as *const Node) as usize);
    }
}
