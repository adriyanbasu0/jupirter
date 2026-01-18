use crate::ast::*;
use crate::lexer::{Token, TokenKind};
use std::fmt;

pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: (usize, usize),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Parse error at {}..{}: {}",
            self.span.0, self.span.1, self.message
        )
    }
}

impl std::error::Error for ParseError {}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn current_kind(&self) -> TokenKind {
        self.current()
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::Error)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&Token, ParseError> {
        if self.current_kind() == kind {
            let token = &self.tokens[self.pos];
            self.pos += 1;
            Ok(token)
        } else {
            let span = self
                .current()
                .map(|t| (t.span.start, t.span.end))
                .unwrap_or((0, 0));
            Err(ParseError {
                message: format!("Expected {:?}, got {:?}", kind, self.current_kind()),
                span,
            })
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        while self.current().is_some() {
            items.push(self.parse_item()?);
        }
        Ok(Program { items })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.current_kind() {
            TokenKind::Fn => self.parse_function(),
            TokenKind::Struct => self.parse_struct(),
            TokenKind::Union => self.parse_union(),
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Const => self.parse_const_decl(),
            TokenKind::Var => self.parse_var_decl(),
            TokenKind::At => {
                self.pos += 1;
                match self.current() {
                    Some(Token {
                        kind: TokenKind::Identifier,
                        ..
                    }) => {
                        let attr_name = self.tokens[self.pos].text.clone();
                        self.pos += 1;
                        if attr_name == "entry" {
                            self.expect(TokenKind::LParen)?;
                            let entry_func_name = if self.current_kind() == TokenKind::Identifier {
                                let name = self.tokens[self.pos].text.clone();
                                self.pos += 1;
                                Some(name)
                            } else {
                                None
                            };
                            self.expect(TokenKind::RParen)?;
                            self.expect(TokenKind::Fn)?;
                            let name = match self.current() {
                                Some(Token {
                                    kind: TokenKind::Identifier,
                                    ..
                                }) => {
                                    let name = self.tokens[self.pos].clone();
                                    self.pos += 1;
                                    name.text.clone()
                                }
                                _ => {
                                    return Err(ParseError {
                                        message: "Expected function name".to_string(),
                                        span: self
                                            .current()
                                            .map(|t| (t.span.start, t.span.end))
                                            .unwrap_or((0, 0)),
                                    });
                                }
                            };
                            self.expect(TokenKind::LParen)?;
                            let mut params = Vec::new();
                            if self.current_kind() != TokenKind::RParen {
                                loop {
                                    let param_name = match self.current() {
                                        Some(Token {
                                            kind: TokenKind::Identifier,
                                            ..
                                        }) => {
                                            let name = self.tokens[self.pos].clone();
                                            self.pos += 1;
                                            name.text.clone()
                                        }
                                        _ => {
                                            return Err(ParseError {
                                                message: "Expected parameter name".to_string(),
                                                span: self
                                                    .current()
                                                    .map(|t| (t.span.start, t.span.end))
                                                    .unwrap_or((0, 0)),
                                            });
                                        }
                                    };
                                    self.expect(TokenKind::Colon)?;
                                    let param_type = self.parse_type()?;
                                    params.push(Param {
                                        name: param_name,
                                        ty: Box::new(param_type),
                                    });
                                    if self.current_kind() == TokenKind::Comma {
                                        self.pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                            }
                            self.expect(TokenKind::RParen)?;
                            if self.current_kind() != TokenKind::Arrow {
                                return Err(ParseError {
                                    message: "Functions with @entry attribute must specify return type with ->".to_string(),
                                    span: self
                                        .current()
                                        .map(|t| (t.span.start, t.span.end))
                                        .unwrap_or((0, 0)),
                                });
                            }
                            self.pos += 1;
                            let return_type = self.parse_type()?;
                            let attrs = vec![FunctionAttribute::Entry(entry_func_name)];
                            self.expect(TokenKind::LBrace)?;
                            let mut body = Vec::new();
                            while self.current_kind() != TokenKind::RBrace {
                                body.push(self.parse_stmt()?);
                            }
                            self.expect(TokenKind::RBrace)?;
                            Ok(Item::Function(Function {
                                name,
                                params,
                                return_type: Box::new(return_type),
                                body,
                                attrs,
                            }))
                        } else {
                            Err(ParseError {
                                message: format!("Unknown attribute: @{}", attr_name),
                                span: self
                                    .current()
                                    .map(|t| (t.span.start, t.span.end))
                                    .unwrap_or((0, 0)),
                            })
                        }
                    }
                    _ => Err(ParseError {
                        message: "Expected attribute name".to_string(),
                        span: self
                            .current()
                            .map(|t| (t.span.start, t.span.end))
                            .unwrap_or((0, 0)),
                    }),
                }
            }
            _ => Err(ParseError {
                message: format!("Unexpected token in item: {:?}", self.current_kind()),
                span: self
                    .current()
                    .map(|t| (t.span.start, t.span.end))
                    .unwrap_or((0, 0)),
            }),
        }
    }

    fn parse_function(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Fn)?;

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected function name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if self.current_kind() != TokenKind::RParen {
            loop {
                let param_name = match self.current() {
                    Some(Token {
                        kind: TokenKind::Identifier,
                        ..
                    }) => {
                        let name = self.tokens[self.pos].clone();
                        self.pos += 1;
                        name.text.clone()
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected parameter name".to_string(),
                            span: self
                                .current()
                                .map(|t| (t.span.start, t.span.end))
                                .unwrap_or((0, 0)),
                        });
                    }
                };
                self.expect(TokenKind::Colon)?;
                let param_type = self.parse_type()?;
                params.push(Param {
                    name: param_name,
                    ty: Box::new(param_type),
                });

                if self.current_kind() == TokenKind::Comma {
                    self.pos += 1;
                } else {
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;

        let return_type = if self.current_kind() == TokenKind::Arrow {
            self.pos += 1;
            self.parse_type()?
        } else if self.current_kind().is_type() {
            self.parse_type()?
        } else {
            Type::I32
        };

        let attrs = self.parse_function_attributes()?;

        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while self.current_kind() != TokenKind::RBrace {
            body.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;

        Ok(Item::Function(Function {
            name,
            params,
            return_type: Box::new(return_type),
            body,
            attrs,
        }))
    }

    fn parse_function_attributes(&mut self) -> Result<Vec<FunctionAttribute>, ParseError> {
        let mut attrs = Vec::new();
        loop {
            if self.current_kind() == TokenKind::Noreturn {
                attrs.push(FunctionAttribute::Noreturn);
                self.pos += 1;
            } else if self.current_kind() == TokenKind::At {
                self.pos += 1;
                match self.current() {
                    Some(Token {
                        kind: TokenKind::Identifier,
                        ..
                    }) => {
                        let attr_name = self.tokens[self.pos].text.clone();
                        self.pos += 1;
                        if attr_name == "entry" {
                            self.expect(TokenKind::LParen)?;
                            let entry_func_name = if self.current_kind() == TokenKind::Identifier {
                                let name = self.tokens[self.pos].text.clone();
                                self.pos += 1;
                                Some(name)
                            } else {
                                None
                            };
                            self.expect(TokenKind::RParen)?;
                            attrs.push(FunctionAttribute::Entry(entry_func_name));
                        } else {
                            return Err(ParseError {
                                message: format!("Unknown attribute: @{}", attr_name),
                                span: self
                                    .current()
                                    .map(|t| (t.span.start, t.span.end))
                                    .unwrap_or((0, 0)),
                            });
                        }
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected attribute name".to_string(),
                            span: self
                                .current()
                                .map(|t| (t.span.start, t.span.end))
                                .unwrap_or((0, 0)),
                        });
                    }
                }
            } else {
                break;
            }
        }
        Ok(attrs)
    }

    fn parse_struct(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Struct)?;

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected struct name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while self.current_kind() != TokenKind::RBrace {
            let field_name = match self.current() {
                Some(Token {
                    kind: TokenKind::Identifier,
                    ..
                }) => {
                    let name = self.tokens[self.pos].clone();
                    self.pos += 1;
                    name.text.clone()
                }
                _ => {
                    return Err(ParseError {
                        message: "Expected field name".to_string(),
                        span: self
                            .current()
                            .map(|t| (t.span.start, t.span.end))
                            .unwrap_or((0, 0)),
                    });
                }
            };
            self.expect(TokenKind::Colon)?;
            let field_type = self.parse_type()?;
            self.expect(TokenKind::Semi)?;
            fields.push(StructField {
                name: field_name,
                ty: Box::new(field_type),
            });
        }
        self.expect(TokenKind::RBrace)?;

        Ok(Item::Struct(Struct { name, fields }))
    }

    fn parse_union(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Union)?;

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected union name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        self.expect(TokenKind::LBrace)?;
        let mut variants = Vec::new();
        while self.current_kind() != TokenKind::RBrace {
            let var_name = match self.current() {
                Some(Token {
                    kind: TokenKind::Identifier,
                    ..
                }) => {
                    let name = self.tokens[self.pos].clone();
                    self.pos += 1;
                    name.text.clone()
                }
                _ => {
                    return Err(ParseError {
                        message: "Expected variant name".to_string(),
                        span: self
                            .current()
                            .map(|t| (t.span.start, t.span.end))
                            .unwrap_or((0, 0)),
                    });
                }
            };
            self.expect(TokenKind::Colon)?;
            let var_type = self.parse_type()?;
            self.expect(TokenKind::Semi)?;
            variants.push(UnionVariant {
                name: var_name,
                ty: Box::new(var_type),
            });
        }
        self.expect(TokenKind::RBrace)?;

        Ok(Item::Union(Union { name, variants }))
    }

    fn parse_enum(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Enum)?;

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected enum name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        self.expect(TokenKind::LBrace)?;
        let mut variants = Vec::new();
        let mut next_value = 0i64;
        while self.current_kind() != TokenKind::RBrace {
            let var_name = match self.current() {
                Some(Token {
                    kind: TokenKind::Identifier,
                    ..
                }) => {
                    let name = self.tokens[self.pos].clone();
                    self.pos += 1;
                    name.text.clone()
                }
                _ => {
                    return Err(ParseError {
                        message: "Expected variant name".to_string(),
                        span: self
                            .current()
                            .map(|t| (t.span.start, t.span.end))
                            .unwrap_or((0, 0)),
                    });
                }
            };

            let value = if self.current_kind() == TokenKind::Eq {
                self.pos += 1;
                match self.current() {
                    Some(Token {
                        kind: TokenKind::Integer,
                        ..
                    }) => {
                        let s = self.tokens[self.pos].text.clone();
                        let val: i64 = s.parse().unwrap_or(0);
                        self.pos += 1;
                        val
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected integer value".to_string(),
                            span: self
                                .current()
                                .map(|t| (t.span.start, t.span.end))
                                .unwrap_or((0, 0)),
                        });
                    }
                }
            } else {
                let val = next_value;
                next_value += 1;
                val
            };

            if self.current_kind() == TokenKind::Comma {
                self.pos += 1;
            }
            variants.push(EnumVariant {
                name: var_name,
                value: Some(value),
            });
        }
        self.expect(TokenKind::RBrace)?;

        Ok(Item::Enum(Enum { name, variants }))
    }

    fn parse_const_decl(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Const)?;

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected const name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        let ty = if self.current_kind() == TokenKind::Colon {
            self.pos += 1;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = Box::new(self.parse_expr()?);
        self.expect(TokenKind::Semi)?;

        Ok(Item::Const(ConstDecl { name, ty, value }))
    }

    fn parse_var_decl(&mut self) -> Result<Item, ParseError> {
        self.expect(TokenKind::Var)?;

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected var name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        let ty = if self.current_kind() == TokenKind::Colon {
            self.pos += 1;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = Box::new(self.parse_expr()?);
        self.expect(TokenKind::Semi)?;

        Ok(Item::Var(VarDecl { name, ty, value }))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.current_kind() {
            TokenKind::Const => self.parse_const_stmt(),
            TokenKind::LBrace => self.parse_block_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Break => {
                self.pos += 1;
                self.expect(TokenKind::Semi)?;
                Ok(Stmt::Break)
            }
            TokenKind::Continue => {
                self.pos += 1;
                self.expect(TokenKind::Semi)?;
                Ok(Stmt::Continue)
            }
            TokenKind::Asm => self.parse_asm_stmt(),
            TokenKind::Defer => self.parse_defer_stmt(),
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Identifier => {
                let lookahead = self.pos + 1;
                if lookahead < self.tokens.len() && self.tokens[lookahead].kind == TokenKind::Colon
                {
                    self.parse_let_stmt()
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(TokenKind::Semi)?;
                    Ok(Stmt::Expr(expr))
                }
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semi)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParseError> {
        let is_const = match self.current_kind() {
            TokenKind::Let => {
                self.pos += 1;
                false
            }
            TokenKind::Const => {
                self.pos += 1;
                true
            }
            TokenKind::Var => {
                self.pos += 1;
                false
            }
            _ => false,
        };

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected variable name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        self.expect(TokenKind::Eq)?;
        let value = Box::new(self.parse_expr()?);
        self.expect(TokenKind::Semi)?;

        Ok(Stmt::Let(LetStmt {
            name,
            ty: Some(Box::new(ty)),
            value,
            is_const,
        }))
    }

    fn parse_const_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::Const)?;

        let name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].clone();
                self.pos += 1;
                name.text.clone()
            }
            _ => {
                return Err(ParseError {
                    message: "Expected const name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        let ty = if self.current_kind() == TokenKind::Colon {
            self.pos += 1;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = Box::new(self.parse_expr()?);
        self.expect(TokenKind::Semi)?;

        Ok(Stmt::Const(ConstStmt { name, ty, value }))
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while self.current_kind() != TokenKind::RBrace {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Stmt::Block(stmts))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::If)?;

        let condition = Box::new(self.parse_expr()?);

        self.expect(TokenKind::LBrace)?;
        let mut then_branch = Vec::new();
        while self.current_kind() != TokenKind::RBrace {
            then_branch.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;

        let else_branch = if self.current_kind() == TokenKind::Else {
            self.pos += 1;
            if self.current_kind() == TokenKind::If {
                Some(vec![self.parse_stmt()?])
            } else {
                self.expect(TokenKind::LBrace)?;
                let mut else_stmts = Vec::new();
                while self.current_kind() != TokenKind::RBrace {
                    else_stmts.push(self.parse_stmt()?);
                }
                self.expect(TokenKind::RBrace)?;
                Some(else_stmts)
            }
        } else {
            None
        };

        Ok(Stmt::If(IfStmt {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::While)?;

        let condition = Box::new(self.parse_expr()?);

        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while self.current_kind() != TokenKind::RBrace {
            body.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;

        Ok(Stmt::While(WhileStmt { condition, body }))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::For)?;

        self.expect(TokenKind::LParen)?;

        let init = Box::new(self.parse_stmt()?);
        let condition = Box::new(self.parse_expr()?);
        self.expect(TokenKind::Semi)?;

        let update = Box::new(self.parse_stmt()?);

        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while self.current_kind() != TokenKind::RBrace {
            body.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;

        Ok(Stmt::For(ForStmt {
            init,
            condition,
            update,
            body,
        }))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::Return)?;

        if self.current_kind() == TokenKind::Semi {
            self.pos += 1;
            Ok(Stmt::Return(None))
        } else {
            let value = Box::new(self.parse_expr()?);
            self.expect(TokenKind::Semi)?;
            Ok(Stmt::Return(Some(*value)))
        }
    }

    fn parse_asm_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::Asm)?;

        let template = match self.current() {
            Some(Token {
                kind: TokenKind::String,
                ..
            }) => {
                let s = self.tokens[self.pos].text.clone();
                self.pos += 1;
                s
            }
            _ => {
                return Err(ParseError {
                    message: "Expected asm template".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };

        let mut inputs = Vec::new();
        let mut outputs = Vec::new();
        let mut clobbers = Vec::new();

        if self.current_kind() == TokenKind::Colon {
            self.pos += 1;
            while self.current_kind() != TokenKind::Colon && self.current_kind() != TokenKind::Semi
            {
                let constraint = match self.current() {
                    Some(Token {
                        kind: TokenKind::String,
                        ..
                    }) => {
                        let c = self.tokens[self.pos].text.clone();
                        self.pos += 1;
                        c
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected constraint string".to_string(),
                            span: self
                                .current()
                                .map(|t| (t.span.start, t.span.end))
                                .unwrap_or((0, 0)),
                        });
                    }
                };
                self.expect(TokenKind::LParen)?;
                let expr = Box::new(self.parse_expr()?);
                self.expect(TokenKind::RParen)?;
                outputs.push(AsmOperand { constraint, expr });

                if self.current_kind() == TokenKind::Comma {
                    self.pos += 1;
                }
            }
        }

        if self.current_kind() == TokenKind::Colon {
            self.pos += 1;
            while self.current_kind() != TokenKind::Colon && self.current_kind() != TokenKind::Semi
            {
                let constraint = match self.current() {
                    Some(Token {
                        kind: TokenKind::String,
                        ..
                    }) => {
                        let c = self.tokens[self.pos].text.clone();
                        self.pos += 1;
                        c
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected constraint string".to_string(),
                            span: self
                                .current()
                                .map(|t| (t.span.start, t.span.end))
                                .unwrap_or((0, 0)),
                        });
                    }
                };
                self.expect(TokenKind::LParen)?;
                let expr = Box::new(self.parse_expr()?);
                self.expect(TokenKind::RParen)?;
                inputs.push(AsmOperand { constraint, expr });

                if self.current_kind() == TokenKind::Comma {
                    self.pos += 1;
                }
            }
        }

        if self.current_kind() == TokenKind::Colon {
            self.pos += 1;
            while self.current_kind() != TokenKind::Semi {
                let clobber = match self.current() {
                    Some(Token {
                        kind: TokenKind::String,
                        ..
                    }) => {
                        let c = self.tokens[self.pos].text.clone();
                        self.pos += 1;
                        c
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected clobber string".to_string(),
                            span: self
                                .current()
                                .map(|t| (t.span.start, t.span.end))
                                .unwrap_or((0, 0)),
                        });
                    }
                };
                clobbers.push(clobber);

                if self.current_kind() == TokenKind::Comma {
                    self.pos += 1;
                }
            }
        }

        self.expect(TokenKind::Semi)?;

        Ok(Stmt::Asm(AsmStmt {
            template,
            inputs,
            outputs,
            clobbers,
        }))
    }

    fn parse_defer_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(TokenKind::Defer)?;
        let stmt = Box::new(self.parse_stmt()?);
        Ok(Stmt::Defer(stmt))
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.current_kind() {
            TokenKind::Void => {
                self.pos += 1;
                Ok(Type::Void)
            }
            TokenKind::Bool => {
                self.pos += 1;
                Ok(Type::Bool)
            }
            TokenKind::I8 => {
                self.pos += 1;
                Ok(Type::I8)
            }
            TokenKind::I16 => {
                self.pos += 1;
                Ok(Type::I16)
            }
            TokenKind::I32 => {
                self.pos += 1;
                Ok(Type::I32)
            }
            TokenKind::I64 => {
                self.pos += 1;
                Ok(Type::I64)
            }
            TokenKind::U8 => {
                self.pos += 1;
                Ok(Type::U8)
            }
            TokenKind::U16 => {
                self.pos += 1;
                Ok(Type::U16)
            }
            TokenKind::U32 => {
                self.pos += 1;
                Ok(Type::U32)
            }
            TokenKind::U64 => {
                self.pos += 1;
                Ok(Type::U64)
            }
            TokenKind::F32 => {
                self.pos += 1;
                Ok(Type::F32)
            }
            TokenKind::F64 => {
                self.pos += 1;
                Ok(Type::F64)
            }
            TokenKind::Usize => {
                self.pos += 1;
                Ok(Type::Usize)
            }
            TokenKind::Isize => {
                self.pos += 1;
                Ok(Type::Isize)
            }
            TokenKind::Star => {
                self.pos += 1;
                let inner = self.parse_type()?;
                if self.current_kind() == TokenKind::Const {
                    self.pos += 1;
                    Ok(Type::ConstPtr(Box::new(inner)))
                } else {
                    Ok(Type::MutPtr(Box::new(inner)))
                }
            }
            TokenKind::Identifier => {
                let name = self.tokens[self.pos].text.clone();

                if name.len() >= 2 && name.len() <= 4 {
                    let first_char = name.chars().next().unwrap();
                    let rest = &name[1..];

                    if (first_char == 'u' || first_char == 'i')
                        && rest.chars().all(|c| c.is_ascii_digit())
                    {
                        let width: u16 = rest.parse().unwrap_or(0);

                        if width >= 1 && width <= 256 {
                            let is_signed = first_char == 'i';
                            self.pos += 1;
                            return Ok(Type::BitInt(width as u8, is_signed));
                        }
                    }
                }

                self.pos += 1;
                Ok(Type::Named(name))
            }
            _ => Err(ParseError {
                message: format!("Unexpected token in type: {:?}", self.current_kind()),
                span: self
                    .current()
                    .map(|t| (t.span.start, t.span.end))
                    .unwrap_or((0, 0)),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assign_expr()
    }

    fn parse_assign_expr(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_logical_or_expr()?;

        if self.current_kind() == TokenKind::Eq {
            self.pos += 1;
            let rhs = Box::new(self.parse_assign_expr()?);
            Ok(Expr::Assign(Box::new(lhs), rhs))
        } else {
            Ok(lhs)
        }
    }

    fn parse_logical_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_logical_and_expr()?;

        while self.current_kind() == TokenKind::OrOr {
            self.pos += 1;
            let rhs = Box::new(self.parse_logical_and_expr()?);
            lhs = Expr::Binary(BinaryOp::LogicalOr, Box::new(lhs), rhs);
        }

        Ok(lhs)
    }

    fn parse_logical_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_bitwise_or_expr()?;

        while self.current_kind() == TokenKind::AndAnd {
            self.pos += 1;
            let rhs = Box::new(self.parse_bitwise_or_expr()?);
            lhs = Expr::Binary(BinaryOp::LogicalAnd, Box::new(lhs), rhs);
        }

        Ok(lhs)
    }

    fn parse_bitwise_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_bitwise_xor_expr()?;

        while self.current_kind() == TokenKind::Pipe {
            self.pos += 1;
            let rhs = Box::new(self.parse_bitwise_xor_expr()?);
            lhs = Expr::Binary(BinaryOp::BitOr, Box::new(lhs), rhs);
        }

        Ok(lhs)
    }

    fn parse_bitwise_xor_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_bitwise_and_expr()?;

        while self.current_kind() == TokenKind::Caret {
            self.pos += 1;
            let rhs = Box::new(self.parse_bitwise_and_expr()?);
            lhs = Expr::Binary(BinaryOp::BitXor, Box::new(lhs), rhs);
        }

        Ok(lhs)
    }

    fn parse_bitwise_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_equality_expr()?;

        while self.current_kind() == TokenKind::Ampersand {
            self.pos += 1;
            let rhs = Box::new(self.parse_equality_expr()?);
            lhs = Expr::Binary(BinaryOp::BitAnd, Box::new(lhs), rhs);
        }

        Ok(lhs)
    }

    fn parse_equality_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_relational_expr()?;

        loop {
            lhs = match self.current_kind() {
                TokenKind::EqEq => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Eq,
                        Box::new(lhs),
                        Box::new(self.parse_relational_expr()?),
                    )
                }
                TokenKind::NotEq => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Neq,
                        Box::new(lhs),
                        Box::new(self.parse_relational_expr()?),
                    )
                }
                _ => break,
            };
        }

        Ok(lhs)
    }

    fn parse_relational_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_shift_expr()?;

        loop {
            lhs = match self.current_kind() {
                TokenKind::Lt => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Lt,
                        Box::new(lhs),
                        Box::new(self.parse_shift_expr()?),
                    )
                }
                TokenKind::Gt => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Gt,
                        Box::new(lhs),
                        Box::new(self.parse_shift_expr()?),
                    )
                }
                TokenKind::LtEq => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::LtEq,
                        Box::new(lhs),
                        Box::new(self.parse_shift_expr()?),
                    )
                }
                TokenKind::GtEq => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::GtEq,
                        Box::new(lhs),
                        Box::new(self.parse_shift_expr()?),
                    )
                }
                _ => break,
            };
        }

        Ok(lhs)
    }

    fn parse_shift_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_additive_expr()?;

        while self.current_kind() == TokenKind::LShift || self.current_kind() == TokenKind::RShift {
            let op = if self.current_kind() == TokenKind::LShift {
                BinaryOp::LShift
            } else {
                BinaryOp::RShift
            };
            self.pos += 1;
            let rhs = Box::new(self.parse_additive_expr()?);
            lhs = Expr::Binary(op, Box::new(lhs), rhs);
        }

        Ok(lhs)
    }

    fn parse_additive_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_multiplicative_expr()?;

        loop {
            lhs = match self.current_kind() {
                TokenKind::Plus => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Add,
                        Box::new(lhs),
                        Box::new(self.parse_multiplicative_expr()?),
                    )
                }
                TokenKind::Minus => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Sub,
                        Box::new(lhs),
                        Box::new(self.parse_multiplicative_expr()?),
                    )
                }
                _ => break,
            };
        }

        Ok(lhs)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_cast_expr()?;

        loop {
            lhs = match self.current_kind() {
                TokenKind::Star => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Mul,
                        Box::new(lhs),
                        Box::new(self.parse_cast_expr()?),
                    )
                }
                TokenKind::Slash => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Div,
                        Box::new(lhs),
                        Box::new(self.parse_cast_expr()?),
                    )
                }
                TokenKind::Percent => {
                    self.pos += 1;
                    Expr::Binary(
                        BinaryOp::Mod,
                        Box::new(lhs),
                        Box::new(self.parse_cast_expr()?),
                    )
                }
                _ => break,
            };
        }

        Ok(lhs)
    }

    fn parse_cast_expr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary_expr()?;

        while self.current_kind() == TokenKind::As {
            self.pos += 1;
            let target_type = self.parse_type()?;
            expr = Expr::Cast(Box::new(expr), target_type);
        }

        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.current_kind() {
            TokenKind::Minus => {
                self.pos += 1;
                Ok(Expr::Unary(
                    UnaryOp::Neg,
                    Box::new(self.parse_unary_expr()?),
                ))
            }
            TokenKind::Not => {
                self.pos += 1;
                Ok(Expr::Unary(
                    UnaryOp::Not,
                    Box::new(self.parse_unary_expr()?),
                ))
            }
            TokenKind::Tilde => {
                self.pos += 1;
                Ok(Expr::Unary(
                    UnaryOp::BitNot,
                    Box::new(self.parse_unary_expr()?),
                ))
            }
            TokenKind::Star => {
                self.pos += 1;
                Ok(Expr::Unary(
                    UnaryOp::Deref,
                    Box::new(self.parse_unary_expr()?),
                ))
            }
            TokenKind::Ampersand => {
                self.pos += 1;
                Ok(Expr::Unary(
                    UnaryOp::AddrOf,
                    Box::new(self.parse_unary_expr()?),
                ))
            }
            TokenKind::Sizeof => {
                self.pos += 1;
                self.expect(TokenKind::LParen)?;
                let ty = self.parse_type()?;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Sizeof(ty))
            }
            TokenKind::Alignof => {
                self.pos += 1;
                self.expect(TokenKind::LParen)?;
                let ty = self.parse_type()?;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Alignof(ty))
            }
            TokenKind::Offsetof => {
                self.pos += 1;
                self.expect(TokenKind::LParen)?;
                let ty = self.parse_type()?;
                self.expect(TokenKind::Comma)?;
                let field_name = match self.current() {
                    Some(Token {
                        kind: TokenKind::Identifier,
                        ..
                    }) => {
                        let f = self.tokens[self.pos].text.clone();
                        self.pos += 1;
                        f
                    }
                    _ => {
                        return Err(ParseError {
                            message: "Expected field name".to_string(),
                            span: self
                                .current()
                                .map(|t| (t.span.start, t.span.end))
                                .unwrap_or((0, 0)),
                        });
                    }
                };
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Offsetof(ty, field_name))
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            expr = match self.current_kind() {
                TokenKind::LBracket => {
                    self.pos += 1;
                    let index = Box::new(self.parse_expr()?);
                    self.expect(TokenKind::RBracket)?;
                    Expr::Index(Box::new(expr), index)
                }
                TokenKind::Dot => {
                    self.pos += 1;
                    if let Expr::Identifier(ref name) = expr {
                        if name == "syscall" {
                            return self.parse_syscall_call();
                        }
                    }
                    let field = match self.current() {
                        Some(Token {
                            kind: TokenKind::Identifier,
                            ..
                        }) => {
                            let f = self.tokens[self.pos].text.clone();
                            self.pos += 1;
                            f
                        }
                        _ => {
                            return Err(ParseError {
                                message: "Expected field name".to_string(),
                                span: self
                                    .current()
                                    .map(|t| (t.span.start, t.span.end))
                                    .unwrap_or((0, 0)),
                            });
                        }
                    };
                    Expr::Field(Box::new(expr), field)
                }
                TokenKind::Arrow => {
                    self.pos += 1;
                    let field = match self.current() {
                        Some(Token {
                            kind: TokenKind::Identifier,
                            ..
                        }) => {
                            let f = self.tokens[self.pos].text.clone();
                            self.pos += 1;
                            f
                        }
                        _ => {
                            return Err(ParseError {
                                message: "Expected field name".to_string(),
                                span: self
                                    .current()
                                    .map(|t| (t.span.start, t.span.end))
                                    .unwrap_or((0, 0)),
                            });
                        }
                    };
                    Expr::PtrField(Box::new(expr), field)
                }
                TokenKind::LParen => {
                    self.pos += 1;
                    let mut args = Vec::new();
                    if self.current_kind() != TokenKind::RParen {
                        loop {
                            args.push(self.parse_expr()?);
                            if self.current_kind() == TokenKind::Comma {
                                self.pos += 1;
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    Expr::Call(Box::new(expr), args)
                }
                _ => break,
            };
        }

        Ok(expr)
    }

    fn parse_syscall_call(&mut self) -> Result<Expr, ParseError> {
        let method_name = match self.current() {
            Some(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                let name = self.tokens[self.pos].text.clone();
                self.pos += 1;
                name
            }
            _ => {
                return Err(ParseError {
                    message: "Expected syscall method name".to_string(),
                    span: self
                        .current()
                        .map(|t| (t.span.start, t.span.end))
                        .unwrap_or((0, 0)),
                });
            }
        };
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();
        if self.current_kind() != TokenKind::RParen {
            loop {
                args.push(self.parse_expr()?);
                if self.current_kind() == TokenKind::Comma {
                    self.pos += 1;
                } else {
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;
        Ok(Expr::Syscall(method_name, args))
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        match self.current_kind() {
            TokenKind::Integer => {
                let s = self.tokens[self.pos].text.clone();
                let val: i64 = s.parse().unwrap_or(0);
                self.pos += 1;
                Ok(Expr::Literal(Literal::Int(val, IntSuffix::None)))
            }
            TokenKind::HexInteger => {
                let s = self.tokens[self.pos].text.clone();
                let val = i64::from_str_radix(&s[2..].replace("_", ""), 16).unwrap_or(0);
                self.pos += 1;
                Ok(Expr::Literal(Literal::Int(val, IntSuffix::None)))
            }
            TokenKind::OctInteger => {
                let s = self.tokens[self.pos].text.clone();
                let val = i64::from_str_radix(&s[2..].replace("_", ""), 8).unwrap_or(0);
                self.pos += 1;
                Ok(Expr::Literal(Literal::Int(val, IntSuffix::None)))
            }
            TokenKind::BinInteger => {
                let s = self.tokens[self.pos].text.clone();
                let val = i64::from_str_radix(&s[2..].replace("_", ""), 2).unwrap_or(0);
                self.pos += 1;
                Ok(Expr::Literal(Literal::Int(val, IntSuffix::None)))
            }
            TokenKind::String => {
                let s = self.tokens[self.pos].text.clone();
                let bytes = s[1..s.len() - 1].as_bytes().to_vec();
                self.pos += 1;
                Ok(Expr::Literal(Literal::String(bytes)))
            }
            TokenKind::Char => {
                let s = self.tokens[self.pos].text.clone();
                self.pos += 1;
                Ok(Expr::Literal(Literal::Char(s.as_bytes()[0])))
            }
            TokenKind::True => {
                self.pos += 1;
                Ok(Expr::Literal(Literal::Bool(true)))
            }
            TokenKind::False => {
                self.pos += 1;
                Ok(Expr::Literal(Literal::Bool(false)))
            }
            TokenKind::Alloc => {
                self.pos += 1;
                self.expect(TokenKind::Lt)?;
                let ty = Box::new(self.parse_type()?);
                self.expect(TokenKind::Gt)?;
                let size = Box::new(self.parse_expr()?);
                Ok(Expr::Alloc(ty, size))
            }
            TokenKind::Free => {
                self.pos += 1;
                self.expect(TokenKind::LParen)?;
                let ptr = Box::new(self.parse_expr()?);
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Free(ptr))
            }
            TokenKind::Identifier => {
                let name = self.tokens[self.pos].text.clone();
                self.pos += 1;
                Ok(Expr::Identifier(name))
            }
            TokenKind::Syscall => {
                let name = self.tokens[self.pos].text.clone();
                self.pos += 1;
                Ok(Expr::Identifier(name))
            }
            TokenKind::LParen => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::LBrace => {
                self.expect(TokenKind::LBrace)?;
                let mut stmts = Vec::new();
                let mut result = None;

                while self.current_kind() != TokenKind::RBrace {
                    if self.current_kind() == TokenKind::Return {
                        self.pos += 1;
                        result = Some(Box::new(self.parse_expr()?));
                        self.expect(TokenKind::Semi)?;
                    } else {
                        stmts.push(self.parse_stmt()?);
                    }
                }
                self.expect(TokenKind::RBrace)?;

                Ok(Expr::Block(stmts, result))
            }
            TokenKind::If => self.parse_if_expr(),
            _ => Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", self.current_kind()),
                span: self
                    .current()
                    .map(|t| (t.span.start, t.span.end))
                    .unwrap_or((0, 0)),
            }),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect(TokenKind::If)?;

        let condition = Box::new(self.parse_expr()?);

        self.expect(TokenKind::LBrace)?;
        let then_expr = Box::new(self.parse_expr()?);
        self.expect(TokenKind::RBrace)?;

        self.expect(TokenKind::Else)?;
        self.expect(TokenKind::LBrace)?;
        let else_expr = Box::new(self.parse_expr()?);
        self.expect(TokenKind::RBrace)?;

        Ok(Expr::If(Box::new(IfExpr {
            condition,
            then_expr,
            else_expr,
        })))
    }
}
