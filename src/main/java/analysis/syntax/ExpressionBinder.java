package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;

import static identifiers.TokenType.*;

@Deprecated
public final class ExpressionBinder
{
    private static final ExpressionBind[] binds =
    {
        // Data Types
        new ExpressionBind(INTEGER_TOKEN,
                           new TokenType[]{AND_TOKEN, CARET_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN,
                                           EQUALS_EQUALS_TOKEN, GREATER_EQUALS_TOKEN, GREATER_TOKEN, LESS_EQUALS_TOKEN,
                                           LESS_TOKEN, MINUS_TOKEN, NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, OR_TOKEN,
                                           PERCENT_TOKEN, PLUS_TOKEN, SLASH_TOKEN, STAR_TOKEN, TO_TOKEN,
                                           PLUS_EQUALS_TOKEN, MINUS_EQUALS_TOKEN, STAR_EQUALS_TOKEN,
                                           SLASH_EQUALS_TOKEN, PERCENT_EQUALS_TOKEN, CARET_EQUALS_TOKEN}),
        new ExpressionBind(DOUBLE_TOKEN,
                           new TokenType[]{AND_TOKEN, CARET_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN,
                                           EQUALS_EQUALS_TOKEN, GREATER_EQUALS_TOKEN, GREATER_TOKEN, LESS_EQUALS_TOKEN,
                                           LESS_TOKEN, MINUS_TOKEN, NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, OR_TOKEN,
                                           PERCENT_TOKEN, PLUS_TOKEN, SLASH_TOKEN, STAR_TOKEN, TO_TOKEN,
                                           PLUS_EQUALS_TOKEN, MINUS_EQUALS_TOKEN, STAR_EQUALS_TOKEN,
                                           SLASH_EQUALS_TOKEN, PERCENT_EQUALS_TOKEN, CARET_EQUALS_TOKEN}),
        new ExpressionBind(BOOLEAN_TOKEN,
                           new TokenType[]{AND_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN, EQUALS_EQUALS_TOKEN,
                                           NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, OR_TOKEN}),
        new ExpressionBind(STRING_TOKEN,
                           new TokenType[]{AND_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN, EQUALS_EQUALS_TOKEN,
                                           NOT_EQUALS_TOKEN, OPEN_BRACE_TOKEN, OR_TOKEN, PLUS_TOKEN, PLUS_EQUALS_TOKEN}),
        new ExpressionBind(IDENTIFIER_TOKEN,
                           new TokenType[]{AND_TOKEN, CARET_TOKEN, CLOSE_BRACE_TOKEN, CLOSE_PARENTHESIS_TOKEN,
                                           EQUALS_EQUALS_TOKEN, EQUALS_TOKEN, GREATER_EQUALS_TOKEN, GREATER_TOKEN,
                                           LESS_EQUALS_TOKEN, LESS_TOKEN, MINUS_TOKEN, NOT_EQUALS_TOKEN,
                                           OPEN_BRACE_TOKEN, OR_TOKEN, PERCENT_TOKEN, PLUS_TOKEN, SLASH_TOKEN,
                                           STAR_TOKEN, TO_TOKEN, PLUS_EQUALS_TOKEN, MINUS_EQUALS_TOKEN, STAR_EQUALS_TOKEN,
                                           SLASH_EQUALS_TOKEN, PERCENT_EQUALS_TOKEN, CARET_EQUALS_TOKEN}),

        // Reserved Words
        new ExpressionBind(IF_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, NOT_TOKEN,
                                           STRING_TOKEN}),
        new ExpressionBind(ELSE_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, LOOP_TOKEN,
                                           NOT_TOKEN, OPEN_BRACE_TOKEN, OPEN_PARENTHESIS_TOKEN, STRING_TOKEN}),
        new ExpressionBind(LOOP_TOKEN,
                           new TokenType[]{IDENTIFIER_TOKEN}),
        new ExpressionBind(TO_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN}),
        new ExpressionBind(AND_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, STRING_TOKEN}),
        new ExpressionBind(OR_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, STRING_TOKEN}),

        // Separators
        new ExpressionBind(OPEN_BRACE_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, CLOSE_BRACE_TOKEN, DOUBLE_TOKEN, IDENTIFIER_TOKEN, IF_TOKEN,
                                           INTEGER_TOKEN, LOOP_TOKEN, OPEN_BRACE_TOKEN, OPEN_PARENTHESIS_TOKEN,
                                           STRING_TOKEN}),
        new ExpressionBind(CLOSE_BRACE_TOKEN,
                           new TokenType[]{BOOLEAN_TOKEN, CLOSE_BRACE_TOKEN, DOUBLE_TOKEN, ELSE_TOKEN, IDENTIFIER_TOKEN,
                                           IF_TOKEN, INTEGER_TOKEN, LOOP_TOKEN, OPEN_BRACE_TOKEN, OPEN_PARENTHESIS_TOKEN,
                                           STRING_TOKEN}),
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
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN,
                                           STRING_TOKEN}),
        new ExpressionBind(MINUS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(STAR_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(SLASH_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(PERCENT_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(CARET_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),

        // Conditional Binary Operators
        new ExpressionBind(GREATER_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(GREATER_EQUALS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(LESS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
        new ExpressionBind(LESS_EQUALS_TOKEN,
                           new TokenType[]{DOUBLE_TOKEN, IDENTIFIER_TOKEN, INTEGER_TOKEN, OPEN_PARENTHESIS_TOKEN}),
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

    public static boolean isBindable(Token current, Token next)
    {
        for (ExpressionBind bind : binds)
            if (bind.getTokenType() == current.getType())
                for (TokenType type : bind.getCompatibleTypes())
                    if (type == next.getType())
                        return true;

        return false;
    }

    public static boolean tokensNotBindable(Token current, Token next)
    {
        return !ExpressionBinder.isBindable(current, next) &&
               next.getType() != TokenType.EOF_TOKEN &&
               next.getType() != LINE_BREAK_TOKEN;
    }
}
