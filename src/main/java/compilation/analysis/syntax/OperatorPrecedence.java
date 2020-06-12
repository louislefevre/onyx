package compilation.analysis.syntax;

import types.TokenType;

/**
 * The OperatorPrecedence class is used to identify the precedence of an operator Token.
 * <p>
 * Using pre-defined operator precedence values, the class takes an operators TokenType and returns a number.
 * This can be used to discover which operators take priority when performing mathematical operations.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class OperatorPrecedence
{
    /**
     * Return the operator precedence for a unary operator.
     *
     * @param type The TokenType of the unary operator
     * @return An int representing the operators precedence value
     */
    public static int getUnaryOperatorPrecedence(TokenType type)
    {
        switch (type)
        {
            case PLUS_TOKEN:
            case MINUS_TOKEN:
            case NOT_TOKEN:
                return 6;
            default:
                return 0;
        }
    }

    /**
     * Return the operator precedence for a binary operator.
     *
     * @param type The TokenType of the binary operator
     * @return An int representing the operators precedence value
     */
    public static int getBinaryOperatorPrecedence(TokenType type)
    {
        switch (type)
        {
            case CARET_TOKEN:
                return 6;
            case STAR_TOKEN:
            case SLASH_TOKEN:
            case PERCENT_TOKEN:
                return 5;
            case PLUS_TOKEN:
            case MINUS_TOKEN:
                return 4;
            case EQUALS_EQUALS_TOKEN:
            case NOT_EQUALS_TOKEN:
            case GREATER_TOKEN:
            case LESS_TOKEN:
            case GREATER_EQUALS_TOKEN:
            case LESS_EQUALS_TOKEN:
                return 3;
            case AND_TOKEN:
                return 2;
            case OR_TOKEN:
                return 1;
            default:
                return 0;
        }
    }
}
