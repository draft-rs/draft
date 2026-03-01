use draft_ast::{
    common::{AssignOp, BinOp, Lit, UnOp},
    untyped::{Case, Expr, Pat, UnwrapField},
};
use draft_common::{bail, bug, span::Span};
use draft_lex::tokens::{Token, TokenKind};

/// Imports
use crate::{Parser, errors::ParseError};

/// Expr parsing implementation
impl<'s> Parser<'s> {
    /// Group expression parsing
    fn group(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        self.expect(TokenKind::Lparen);
        let expr = self.expr();
        self.expect(TokenKind::Rparen);
        let end_span = self.prev().span.clone();

        Expr::Paren(start_span + end_span, Box::new(expr))
    }

    /// Variable parsing
    fn variable(&mut self) -> Expr {
        // parsing base identifier
        let start_span = self.peek().span.clone();
        let id = self.expect(TokenKind::Id).lexeme;

        // result node
        let mut result = Expr::Var(start_span.clone(), id);

        // checking for dots and parens
        loop {
            // checking for chain `a.b.c.d`
            if self.check(TokenKind::Dot) {
                self.bump();

                let id = self.expect(TokenKind::Id).lexeme;
                let end_span = self.prev().span.clone();

                result = Expr::Suffix(start_span.clone() + end_span, Box::new(result), id);
                continue;
            }

            // checking for call
            if self.check(TokenKind::Lparen) {
                let args = self.sep_by(
                    TokenKind::Lparen,
                    TokenKind::Rparen,
                    TokenKind::Comma,
                    |p| p.expr(),
                );
                let end_span = self.prev().span.clone();

                result = Expr::Call(start_span.clone() + end_span, Box::new(result), args);
                continue;
            }

            // breaking cycle
            break;
        }
        result
    }

    /// If expression parsing
    fn if_expr(&mut self) -> Expr {
        // Bumping `if`
        let start_span = self.peek().span.clone();
        self.bump();

        // Parsing if block
        let expr = self.expr();
        let block = self.block();

        // Parsing else block
        if self.check(TokenKind::Else) {
            self.bump();

            let branch = if self.check(TokenKind::If) {
                self.if_expr()
            } else {
                self.block()
            };

            let end_span = self.prev().span.clone();
            Expr::If(
                start_span + end_span,
                Box::new(expr),
                Box::new(block),
                Some(Box::new(branch)),
            )
        } else {
            let end_span = self.prev().span.clone();
            Expr::If(start_span + end_span, Box::new(expr), Box::new(block), None)
        }
    }

    /// Function expression parsing
    fn fn_expr(&mut self) -> Expr {
        // Bumping `fn`
        let start_span = self.peek().span.clone();
        self.bump();

        // Collecting params
        let params = self.sep_by(
            TokenKind::Lparen,
            TokenKind::Rparen,
            TokenKind::Comma,
            |p| p.expect(TokenKind::Id).lexeme,
        );
        let body = self.block();
        let end_span = self.prev().span.clone();

        Expr::Function(start_span + end_span, params, Box::new(body))
    }

    /// Enum pattern name parsing
    fn enum_pat_name(&mut self, id: Token) -> Expr {
        // parsing base identifier
        let start_span = id.span.clone();

        // result node
        let mut result = Expr::Var(start_span.clone(), id.lexeme);

        // checking for dots and parens
        loop {
            // checking for chain `a.b.c.d`
            if self.check(TokenKind::Dot) {
                self.bump();

                let id = self.expect(TokenKind::Id).lexeme;
                let end_span = self.prev().span.clone();

                result = Expr::Suffix(start_span.clone() + end_span, Box::new(result), id);
                continue;
            }

            // breaking cycle
            break;
        }
        result
    }

    /// Unwrap pattern parsing
    fn unwrap_pat(&mut self, start_span: Span, en: Expr) -> Pat {
        // Parsing fields
        let fields = self.sep_by(
            TokenKind::Lparen,
            TokenKind::Rparen,
            TokenKind::Comma,
            |p| {
                let span = p.peek().span.clone();

                if p.check(TokenKind::Wildcard) {
                    p.bump();
                    UnwrapField::Wildcard(span)
                } else {
                    let id = p.expect(TokenKind::Id).lexeme;
                    UnwrapField::Field(span, id)
                }
            },
        );

        let end_span = self.prev().span.clone();
        Pat::Unwrap(start_span + end_span, en, fields)
    }

    /// Signle pattern parsing
    fn single_pat(&mut self) -> Pat {
        // if string presented
        if self.check(TokenKind::String) {
            let tk = self.bump();
            Pat::String(tk.span, tk.lexeme)
        }
        // if bool presented
        else if self.check(TokenKind::Bool) {
            let tk = self.bump();
            Pat::Bool(tk.span, tk.lexeme)
        }
        // if number presented
        else if self.check(TokenKind::Number) {
            let tk = self.bump();
            if tk.lexeme.contains(".") {
                Pat::Float(tk.span, tk.lexeme)
            } else {
                Pat::Int(tk.span, tk.lexeme)
            }
        }
        // if wildcard presented
        else if self.check(TokenKind::Wildcard) {
            self.bump();
            Pat::Wildcard
        }
        // if identifier presented
        else {
            // Start span
            let start_span = self.peek().span.clone();
            let id = self.expect(TokenKind::Id);

            // If dot presented -> enum pattern
            if self.check(TokenKind::Dot) {
                // Parsing enum pattern name
                let en = self.enum_pat_name(id);

                // Unwrap pattern
                if self.check(TokenKind::Lparen) {
                    self.unwrap_pat(start_span, en)
                }
                // Variant pattern
                else {
                    let end_span = self.prev().span.clone();
                    Pat::Variant(start_span + end_span, en)
                }
            }
            // if not -> bind pattern
            else {
                let binding = self.expect(TokenKind::Id).lexeme;
                let end_span = self.prev().span.clone();

                Pat::BindTo(start_span + end_span, binding)
            }
        }
    }

    /// Pattern parsing
    fn pat(&mut self) -> Pat {
        // parsing single pattern
        let pat = self.single_pat();

        // cecking if more patterns presented
        if self.check(TokenKind::Bar) {
            // parsing `or` pattern
            self.expect(TokenKind::Bar);

            // left and right pattern
            let a = Box::new(pat);
            let b = Box::new(self.pat());

            Pat::Or(a, b)
        } else {
            pat
        }
    }

    /// Case parsing
    fn case(&mut self) -> Case {
        // Pattern of case
        let start_span = self.peek().span.clone();
        let pat = self.pat();

        // -> { body, ... }
        self.expect(TokenKind::Colon);
        let body = if self.check(TokenKind::Lbrace) {
            self.block()
        } else {
            self.expr()
        };
        let end_span = self.prev().span.clone();

        Case {
            span: start_span + end_span,
            pat,
            body,
        }
    }

    /// Match expression parsing
    fn match_expr(&mut self) -> Expr {
        // `match value { patterns, ... }`
        let start_span = self.peek().span.clone();
        self.expect(TokenKind::Match);
        let value = self.expr();

        // Parsing cases
        let cases = self.sep_by(
            TokenKind::Lbrace,
            TokenKind::Rbrace,
            TokenKind::Comma,
            |p| p.case(),
        );
        let end_span = self.prev().span.clone();

        Expr::Match(start_span + end_span, Box::new(value), cases)
    }

    /// Atom expression parsing
    fn atom(&mut self) -> Expr {
        let tk = self.peek().clone();
        match tk.kind {
            TokenKind::Lparen => self.group(),
            TokenKind::Number => {
                self.bump();
                if tk.lexeme.contains(".") {
                    Expr::Lit(tk.span, Lit::Float(tk.lexeme))
                } else {
                    Expr::Lit(tk.span, Lit::Int(tk.lexeme))
                }
            }
            TokenKind::String => {
                self.bump();
                Expr::Lit(tk.span, Lit::String(tk.lexeme))
            }
            TokenKind::Bool => {
                self.bump();
                Expr::Lit(
                    tk.span,
                    Lit::Bool(match tk.lexeme.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => bug!("non-bool value in bool literal"),
                    }),
                )
            }
            TokenKind::Id => self.variable(),
            TokenKind::If => self.if_expr(),
            TokenKind::Fn => self.fn_expr(),
            TokenKind::Match => self.match_expr(),
            _ => bail!(ParseError::UnexpectedExprToken {
                got: tk.kind,
                src: self.source.clone(),
                span: tk.span.1.into(),
            }),
        }
    }

    /// Unary expression parsing
    fn unary_expr(&mut self) -> Expr {
        if self.check(TokenKind::Minus) || self.check(TokenKind::Bang) {
            let start_span = self.peek().span.clone();

            let op = match self.bump().kind {
                TokenKind::Minus => UnOp::Neg,
                TokenKind::Bang => UnOp::Bang,
                _ => unreachable!(),
            };

            let value = self.unary_expr();
            let end_span = self.prev().span.clone();

            return Expr::Unary(start_span + end_span, Box::new(value), op);
        }

        self.atom()
    }

    /// Factor expression parsing
    fn factor_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.unary_expr();

        while self.check(TokenKind::Star)
            || self.check(TokenKind::Slash)
            || self.check(TokenKind::Percent)
        {
            let op = match self.bump().kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                TokenKind::Percent => BinOp::Mod,
                _ => unreachable!(),
            };

            let right = self.unary_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                op,
            );
        }

        left
    }

    /// Term expression parsing
    fn term_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.factor_expr();

        while self.check(TokenKind::Plus) || self.check(TokenKind::Minus) {
            let op = match self.bump().kind {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => unreachable!(),
            };

            let right = self.factor_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                op,
            );
        }

        left
    }

    /// Compare expression parsing
    fn compare_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.term_expr();

        while self.check(TokenKind::Ge)
            || self.check(TokenKind::Gt)
            || self.check(TokenKind::Le)
            || self.check(TokenKind::Lt)
        {
            let op = match self.bump().kind {
                TokenKind::Ge => BinOp::Ge,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::Le => BinOp::Le,
                TokenKind::Lt => BinOp::Lt,
                _ => unreachable!(),
            };

            let right = self.factor_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                op,
            );
        }

        left
    }

    /// Equality expression parsing
    fn equality_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.compare_expr();

        while self.check(TokenKind::DoubleEq) || self.check(TokenKind::BangEq) {
            let op = match self.bump().kind {
                TokenKind::DoubleEq => BinOp::Eq,
                TokenKind::BangEq => BinOp::Ne,
                _ => unreachable!(),
            };

            let right = self.compare_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                op,
            );
        }

        left
    }

    /// `draftwise and` expression parsing
    fn draftwise_and_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.equality_expr();

        while self.check(TokenKind::Ampersand) {
            self.bump();

            let right = self.equality_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                BinOp::BitAnd,
            );
        }

        left
    }

    /// `draftwise xor` expression parsing
    fn draftwise_xor_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.draftwise_and_expr();

        while self.check(TokenKind::Caret) {
            self.bump();

            let right = self.draftwise_and_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                BinOp::Xor,
            );
        }

        left
    }

    /// `draftwise or` expression parsing
    fn draftwise_or_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.draftwise_xor_expr();

        while self.check(TokenKind::Bar) {
            self.bump();

            let right = self.draftwise_xor_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                BinOp::BitOr,
            );
        }

        left
    }

    /// `Logical and` expression parsing
    fn logical_and_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.draftwise_or_expr();

        while self.check(TokenKind::DoubleAmp) {
            self.bump();

            let right = self.draftwise_or_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                BinOp::And,
            );
        }

        left
    }

    /// `Logical or` expression parsing
    fn logical_or_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.logical_and_expr();

        while self.check(TokenKind::DoubleBar) {
            self.bump();

            let right = self.logical_and_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Bin(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                BinOp::Or,
            );
        }

        left
    }

    /// `Assign` expression parsing
    fn assign_expr(&mut self) -> Expr {
        let start_span = self.peek().span.clone();
        let mut left = self.logical_or_expr();

        while self.check(TokenKind::Eq)
            | self.check(TokenKind::AmpersandEq)
            | self.check(TokenKind::BarEq)
            | self.check(TokenKind::PlusEq)
            | self.check(TokenKind::MinusEq)
            | self.check(TokenKind::StarEq)
            | self.check(TokenKind::SlashEq)
            | self.check(TokenKind::PercentEq)
            | self.check(TokenKind::CaretEq)
        {
            let op = match self.bump().kind {
                TokenKind::Eq => AssignOp::Eq,
                TokenKind::AmpersandEq => AssignOp::AndEq,
                TokenKind::BarEq => AssignOp::OrEq,
                TokenKind::PlusEq => AssignOp::AddEq,
                TokenKind::MinusEq => AssignOp::SubEq,
                TokenKind::StarEq => AssignOp::MulEq,
                TokenKind::SlashEq => AssignOp::DivEq,
                TokenKind::PercentEq => AssignOp::ModEq,
                TokenKind::CaretEq => AssignOp::XorEq,
                _ => unreachable!(),
            };

            let right = self.logical_or_expr();
            let end_span = self.prev().span.clone();

            left = Expr::Assign(
                start_span.clone() + end_span,
                Box::new(left),
                Box::new(right),
                op,
            );
        }

        left
    }

    /// Parses expression
    pub fn expr(&mut self) -> Expr {
        self.assign_expr()
    }
}
