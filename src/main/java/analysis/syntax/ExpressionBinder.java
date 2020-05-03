package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;

import static identifiers.TokenType.*;

public final class ExpressionBinder
{
    private ExpressionBinder()
    {
        throw new UnsupportedOperationException();
    }

    private static final ExpressionBind[] binds =
    {
        // Data Types
        new ExpressionBind(INTEGER_TOKEN,
                           new TokenType[]{CARET_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN, EQUALS_EQUALS_TOKEN,
                                           GREATER_EQUALS_TOKEN, GREATER_TOKEN, LESS_EQUALS_TOKEN, LESS_TOKEN,
                                           MINUS_TOKEN, NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, PERCENT_TOKEN, PLUS_TOKEN,
                                           SLASH_TOKEN, STAR_TOKEN}),
        new ExpressionBind(DOUBLE_TOKEN,
                           new TokenType[]{CARET_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN, EQUALS_EQUALS_TOKEN,
                                           GREATER_EQUALS_TOKEN, GREATER_TOKEN, LESS_EQUALS_TOKEN, LESS_TOKEN,
                                           MINUS_TOKEN, NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, PERCENT_TOKEN, PLUS_TOKEN,
                                           SLASH_TOKEN, STAR_TOKEN}),
        new ExpressionBind(BOOLEAN_TOKEN,
                           new TokenType[]{AND_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN, EQUALS_EQUALS_TOKEN,
                                           NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, OR_TOKEN}),
        new ExpressionBind(STRING_TOKEN,
                           new TokenType[]{CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN, EQUALS_EQUALS_TOKEN,
                                           NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, PLUS_TOKEN}),
        new ExpressionBind(IDENTIFIER_TOKEN,
                           new TokenType[]{AND_TOKEN, CARET_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN,
                                           EQUALS_EQUALS_TOKEN, EQUALS_TOKEN, GREATER_EQUALS_TOKEN, GREATER_TOKEN,
                                           LESS_EQUALS_TOKEN, LESS_TOKEN, MINUS_TOKEN, NOT_EQUALS_TOKEN,
                                           OPEN_BRACE_TOKEN, OR_TOKEN, PERCENT_TOKEN, PLUS_TOKEN, SLASH_TOKEN,
                                           STAR_TOKEN}),

        // Separators
        new ExpressionBind(OPEN_BRACE_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, CLOSE_BRACE_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN,
                                           INTEGER_TOKEN, OPEN_BRACE_TOKEN, OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(CLOSE_BRACE_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, CLOSE_BRACE_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN,
                                           INTEGER_TOKEN, OPEN_BRACE_TOKEN, OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(OPEN_PARENTHESIS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, CLOSE_PARENTHESIS_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN,
                                           INTEGER_TOKEN, MINUS_TOKEN, NOT_TOKEN, OPEN_PARENTHESIS_TOKEN, PLUS_TOKEN,
                                           STRING_TOKEN}),
        new ExpressionBind(CLOSE_PARENTHESIS_TOKEN,
                           new TokenType[]{AND_TOKEN, CARET_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN,
                                           EQUALS_EQUALS_TOKEN, GREATER_EQUALS_TOKEN, GREATER_TOKEN, LESS_EQUALS_TOKEN,
                                           LESS_TOKEN, MINUS_TOKEN, NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, OR_TOKEN,
                                           PERCENT_TOKEN, PLUS_TOKEN, SLASH_TOKEN, STAR_TOKEN}),

        // Unary Operators
        new ExpressionBind(NOT_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, IDENTIFIER_TOKEN}),

        // Mathematical Binary Operators
        new ExpressionBind(PLUS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(MINUS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(STAR_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(SLASH_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(PERCENT_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(CARET_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),

        // Conditional Binary Operators
        new ExpressionBind(AND_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, IDENTIFIER_TOKEN}),
        new ExpressionBind(OR_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, IDENTIFIER_TOKEN}),
        new ExpressionBind(GREATER_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(GREATER_EQUALS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(LESS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(LESS_EQUALS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(EQUALS_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),

        // Assignment Operators
        new ExpressionBind(EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(NOT_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(PLUS_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(MINUS_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(STAR_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(SLASH_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(PERCENT_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(CARET_EQUALS_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN,
                                           OPEN_PARENTHESIS_TOKEN, STRING_TOKEN})
    };

    private static boolean isBindable(Token current, Token next)
    {
        for (ExpressionBind bind : binds)
            if (bind.getTokenType() == current.getTokenType())
                for (TokenType type : bind.getCompatibleTypes())
                    if (type == next.getTokenType())
                        return true;

        return false;
    }

    public static boolean tokensNotBindable(Token current, Token next)
    {
        return !ExpressionBinder.isBindable(current, next) && next.getTokenType() != TokenType.EOF_TOKEN;
    }
}
