import java.util.ArrayList;
import java.util.List;

public class Parser {
    private final List<Token> tokens;
    private int current = 0;
    private boolean hayErrores = false;


    public Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    public List<Statement> parse() {
        List<Statement> statements = new ArrayList<>();
        while (!isAtEnd()) {
            statements.add(declaration());
        }
        return statements;
    }

    private Statement declaration() {
        if (match(TipoToken.FUN)) return funDecl();
        if (match(TipoToken.VAR)) return varDecl();
        return statement();
    }

    private StmtFunction funDecl() {
        Token name = consume(TipoToken.IDENTIFICADOR, "Se esperaba nombre de la función");
        consume(TipoToken.IZQ_PARENTESIS, "Se esperaba '('");
        List<Token> parameters = new ArrayList<>();
        if (!check(TipoToken.DER_PARENTESIS)) {
            do {
                parameters.add(consume(TipoToken.IDENTIFICADOR, "Se esperaba nombre del parámetro"));
            } while (match(TipoToken.COMA));
        }
        consume(TipoToken.DER_PARENTESIS, "Se esperaba ')'");
        StmtBlock body = block();
        return new StmtFunction(name, parameters, body);
    }

    private StmtVar varDecl() {
        Token name = consume(TipoToken.IDENTIFICADOR, "Se esperaba nombre de variable");
        Expression initializer = null;
        if (match(TipoToken.IGUAL)) {
            initializer = expression();
        }
        consume(TipoToken.PUNTO_COMA, "Se esperaba ';'");
        return new StmtVar(name, initializer);
    }

    private Statement statement() {
        if (match(TipoToken.FOR)) return forStmt();
        if (match(TipoToken.IF)) return ifStmt();
        if (match(TipoToken.PRINT)) return printStmt();
        if (match(TipoToken.RETURN)) return returnStmt();
        if (match(TipoToken.WHILE)) return whileStmt();
        if (match(TipoToken.IZQ_LLAVE)) return block();
        return exprStmt();
    }


    private StmtIf ifStmt() {
        consume(TipoToken.IF, "Se esperaba 'if'");
        consume(TipoToken.IZQ_PARENTESIS, "Se esperaba '('");
        Expression condition = expression();
        consume(TipoToken.DER_PARENTESIS, "Se esperaba ')'");
        Statement thenBranch = statement();
        Statement elseBranch = null;
        if (match(TipoToken.ELSE)) {
            elseBranch = statement();
        }
        return new StmtIf(condition, thenBranch, elseBranch);
    }
/*
    private Statement forStmt() {
        consume(TipoToken.IZQ_PARENTESIS, "Se esperaba '(' después de 'for'.");

        // Inicialización
        Statement initializer;
        if (match(TipoToken.PUNTO_COMA)) {
            initializer = null;
        } else if (match(TipoToken.VAR)) {
            initializer = varDecl();
        } else {
            initializer = exprStmt();
        }

        // Condición
        Expression condition = null;
        if (!check(TipoToken.PUNTO_COMA)) {
            condition = expression();
        }
        consume(TipoToken.PUNTO_COMA, "Se esperaba ';' después de la condición.");

        // Incremento
        Expression increment = null;
        if (!check(TipoToken.DER_PARENTESIS)) {
            increment = expression();
        }
        consume(TipoToken.DER_PARENTESIS, "Se esperaba ')' después de las cláusulas del for.");

        // Cuerpo
        Statement body = statement();

        // Agrega incremento al final del cuerpo
        if (increment != null) {
            body = new StmtBlock(List.of(body, new StmtExpression(increment)));
        }

        // Si no hay condición, es true por defecto (bucle infinito)
        if (condition == null) {
            condition = new ExprLiteral(true);
        }

        body = new StmtLoop(condition, body);

        // Agrega inicialización antes del bucle
        if (initializer != null) {
            body = new StmtBlock(List.of(initializer, body));
        }

        return body;
    }
*/

    private StmtLoop whileStmt() {
        consume(TipoToken.WHILE, "Se esperaba 'while'");
        consume(TipoToken.IZQ_PARENTESIS, "Se esperaba '('");
        Expression condition = expression();
        consume(TipoToken.DER_PARENTESIS, "Se esperaba ')'");
        Statement body = statement();
        return new StmtLoop(condition, body);
    }

    private StmtReturn returnStmt() {
        consume(TipoToken.RETURN, "Se esperaba 'return'");
        Expression value = null;
        if (!check(TipoToken.PUNTO_COMA)) {
            value = expression();
        }
        consume(TipoToken.PUNTO_COMA, "Se esperaba ';'");
        return new StmtReturn(value);
    }

    private StmtPrint printStmt() {
        Expression value = expression();
        consume(TipoToken.PUNTO_COMA, "Se esperaba ';'");
        return new StmtPrint(value);
    }

    private StmtExpression exprStmt() {
        Expression expr = expression();
        consume(TipoToken.PUNTO_COMA, "Se esperaba ';'");
        return new StmtExpression(expr);
    }

    private StmtBlock block() {
        List<Statement> statements = new ArrayList<>();
        while (!check(TipoToken.DER_LLAVE) && !isAtEnd()) {
            statements.add(declaration());
        }
        consume(TipoToken.DER_LLAVE, "Se esperaba '}'");
        return new StmtBlock(statements);
    }

    // LAS EXPRESIONES EMPIEZAN AQUI

    private Expression expression() {
        return assignment();
    }

    private Expression assignment() {
        Expression expr = logicOr();

        if (match(TipoToken.IGUAL)) {
            Token equals = previous();
            Expression value = assignment();

            if (expr instanceof ExprVariable) {
                Token name = ((ExprVariable) expr).name;
                return new ExprAssign(name, value);
            }

            throw error(equals, "Asignación inválida");
        }

        return expr;
    }

    private Expression logicOr() {
        Expression expr = logicAnd();

        while (match(TipoToken.OR)) {
            Token operator = previous();
            Expression right = logicAnd();
            expr = new ExprLogical(expr, operator, right);
        }

        return expr;
    }

    private Expression logicAnd() {
        Expression expr = equality();

        while (match(TipoToken.AND)) {
            Token operator = previous();
            Expression right = equality();
            expr = new ExprLogical(expr, operator, right);
        }

        return expr;
    }

    private Expression equality() {
        Expression expr = comparison();

        while (match(TipoToken.IGUALIGUAL, TipoToken.DISTINTO)) {
            Token operator = previous();
            Expression right = comparison();
            expr = new ExprRelational(expr, operator, right);
        }

        return expr;
    }

    private Expression comparison() {
        Expression expr = term();

        while (match(TipoToken.MAYORQUE, TipoToken.MAYORIGUAL, TipoToken.MENORQUE, TipoToken.MENORIGUAL)) {
            Token operator = previous();
            Expression right = term();
            expr = new ExprRelational(expr, operator, right);
        }

        return expr;
    }

    private Expression term() {
        Expression expr = factor();

        while (match(TipoToken.MAS, TipoToken.MENOS)) {
            Token operator = previous();
            Expression right = factor();
            expr = new ExprArithmetic(expr, operator, right);
        }

        return expr;
    }

    private Expression factor() {
        Expression expr = unary();

        while (match(TipoToken.PRODUCTO, TipoToken.ENTRE)) {
            Token operator = previous();
            Expression right = unary();
            expr = new ExprArithmetic(expr, operator, right);
        }

        return expr;
    }

    private Expression unary() {
        if (match(TipoToken.NOT, TipoToken.MENOS, TipoToken.INVERSOR)) {
            Token operator = previous();
            Expression right = unary();
            return new ExprUnary(operator, right);
        }

        return call();
    }

    private Expression call() {
        Expression expr = primary();

        while (true) {
            if (match(TipoToken.IZQ_PARENTESIS)) {
                expr = finishCall(expr);
            } else {
                break;
            }
        }

        return expr;
    }

    private Expression finishCall(Expression callee) {
        List<Expression> arguments = new ArrayList<>();
        if (!check(TipoToken.DER_PARENTESIS)) {
            do {
                arguments.add(expression());
            } while (match(TipoToken.COMA));
        }

        consume(TipoToken.DER_PARENTESIS, "Se esperaba ')' después de los argumentos.");
        return new ExprCallFunction(callee, arguments);
    }

    private Expression primary() {
        if (match(TipoToken.TRUE)) return new ExprLiteral(true);
        if (match(TipoToken.FALSE)) return new ExprLiteral(false);
        if (match(TipoToken.NULL)) return new ExprLiteral(null);
        if (match(TipoToken.INT, TipoToken.FLOAT, TipoToken.CADENA)) {
            return new ExprLiteral(previous().getLiteral());
        }
        if (match(TipoToken.IDENTIFICADOR)) {
            return new ExprVariable(previous());
        }
        if (match(TipoToken.IZQ_PARENTESIS)) {
            Expression expr = expression();
            consume(TipoToken.DER_PARENTESIS, "Se esperaba ')' después de la expresión agrupada");
            return new ExprGrouping(expr);
        }

        throw error(peek(), "Se esperaba una expresión primaria");
    }

    private boolean match(TipoToken... tipos) {
        for (TipoToken tipo : tipos) {
            if (check(tipo)) {
                advance();
                return true;
            }
        }
        return false;
    }

    private boolean check(TipoToken tipo) {
        return !isAtEnd() && peek().getTipo() == tipo;
    }

    private Token consume(TipoToken tipo, String mensaje) {
        if (check(tipo)) return advance();
        throw error(peek(), mensaje);
    }

    private Token advance() {
        if (!isAtEnd()) current++;
        return previous();
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }

    private boolean isAtEnd() {
        return peek().getTipo() == TipoToken.EOF;
    }

    private RuntimeException error(Token token, String mensaje) {
        hayErrores = true;
        return new RuntimeException("Error en la línea " + token.getLinea() + ": " + mensaje + ". Token recibido: " + token.getTipo());
    }

    public boolean tuvoErrores() {
        return hayErrores;
    }

}