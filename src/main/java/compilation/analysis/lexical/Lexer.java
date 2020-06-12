package compilation.analysis.lexical;

import errors.ErrorHandler;
import errors.LexicalError;
import source.SourceInput;
import types.TokenType;

import java.util.ArrayList;
import java.util.List;

import static compilation.analysis.lexical.Syntax.*;
import static errors.LexicalError.badCharacter;
import static errors.LexicalError.incompleteString;
import static errors.LexicalError.invalidDouble;
import static errors.LexicalError.invalidInt;
import static types.TokenType.*;

/**
 * The Lexer class is responsible for performing lexical analysis on a string of text, identifying each character
 * one by one.
 * <p>
 * It creates Tokens that represent each individual character, ultimately producing a List of Token objects
 * ordered sequentially. During this stage any characters that are unrecognised by source language are identified
 * and reported as an error.
 * <p>
 * The contents of the String contained within SourceInput are parsed character by character, with each one
 * being examined individually in order to gather information about its general characteristics (e.g. syntax, value,
 * type, and position in the text). A Token object is then generated to store this data, with a new instance being
 * created to represent every character. These are incrementally and sequentially added to a List, which is the
 * single output of this class.
 * <p>
 * Any compilation errors that occur during this stage are passed to the ErrorHandler in the form of LexicalError
 * objects.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class Lexer
{
    private final SourceInput sourceInput;
    private final ErrorHandler errorHandler;
    private int position;

    /**
     * Constructs a Lexer object initialised with contents of sourceInput.
     * <p>
     * The Lexer always begins at position 0 in the text, and adds any errors to the errorHandler.
     *
     * @param sourceInput The source code input to be lexed
     * @param errorHandler The ErrorHandler to store any errors that occur
     */
    public Lexer(SourceInput sourceInput, ErrorHandler errorHandler)
    {
        this.sourceInput = sourceInput;
        this.errorHandler = errorHandler;
        this.position = 0;
    }

    /**
     * Returns a List of Token objects generated from sourceInput.
     * <p>
     * The contents of sourceInput are lexed into Token objects, and added sequentially into a List.
     *
     * @return A List of Token objects
     */
    public List<Token> getTokens()
    {
        return lexTokens();
    }

    private List<Token> lexTokens()
    {
        List<Token> tokens = new ArrayList<>();
        Token token;
        TokenType tokenType;

        do
        {
            token = nextToken();
            tokenType = token.getType();

            if (tokenType != BAD_TOKEN && tokenType != WHITE_SPACE_TOKEN && tokenType != COMMENT_TOKEN)
                tokens.add(token);
        }
        while (token.getType() != EOF_TOKEN);

        return tokens;
    }

    private Token nextToken()
    {
        if (position >= sourceInput.length())
            return endToken();
        else if (isLineBreak(currentChar()))
            return lineBreakToken();
        else if (isWhitespace(currentChar()))
            return whitespaceToken();
        else if (isDigit(currentChar()))
            return integerToken();
        else if (isLetter(currentChar()))
            return letterToken();
        return operatorToken();
    }

    private Token endToken()
    {
        return new Token(EOF_TOKEN, EOF_SYNTAX, position);
    }

    private Token lineBreakToken()
    {
        if (currentChar().equals("\r") && nextChar().equals("\n")) // Handles Windows line breaks
            return new Token(LINE_BREAK_TOKEN, LINE_BREAK_SYNTAX, currentPositionThenNext(2));
        return new Token(LINE_BREAK_TOKEN, LINE_BREAK_SYNTAX, currentPositionThenNext(1));
    }

    private Token whitespaceToken()
    {
        int startPos = position;

        while (isWhitespace(currentChar()) && !isLineBreak(currentChar()))
            nextPosition();

        String syntax = sourceInput.substring(startPos, position);

        return new Token(WHITE_SPACE_TOKEN, syntax, startPos);
    }

    private Token integerToken()
    {
        int startPos = position;

        while (isDigit(currentChar()))
            currentPositionThenNext(1);

        if (currentChar().equals(DECIMAL_POINT_SYNTAX))
            return doubleToken(startPos);

        String syntax = sourceInput.substring(startPos, position);
        int value = 0;

        if (isIntegerParsable(syntax))
        {
            value = Integer.parseInt(syntax);
        }
        else
        {
            LexicalError error = invalidInt(syntax, startPos, position - startPos);
            errorHandler.add(error);
        }


        return new Token(INTEGER_TOKEN, syntax, value, startPos);
    }

    private Token doubleToken(int startPos)
    {
        do // 'do' to skip the decimal point
        {
            currentPositionThenNext(1);
        }
        while (isDigit(currentChar()));

        String syntax = sourceInput.substring(startPos, position);
        double value = 0;

        if (isDoubleParsable(syntax))
        {
            value = Double.parseDouble(syntax);
        }
        else
        {
            LexicalError error = invalidDouble(syntax, startPos, position - startPos);
            errorHandler.add(error);
        }

        return new Token(DOUBLE_TOKEN, syntax, value, startPos);
    }

    private Token letterToken()
    {
        int startPos = position;

        while (isLetter(currentChar()))
            nextPosition();

        String syntax = sourceInput.substring(startPos, position).toLowerCase();

        return getKeywordToken(syntax, startPos);
    }

    private static Token getKeywordToken(String text, int pos)
    {
        switch (text)
        {
            case TRUE_SYNTAX:
                return new Token(BOOLEAN_TOKEN, TRUE_SYNTAX, true, pos);
            case FALSE_SYNTAX:
                return new Token(BOOLEAN_TOKEN, FALSE_SYNTAX, false, pos);
            case AND_SYNTAX:
                return new Token(AND_TOKEN, AND_SYNTAX, pos);
            case OR_SYNTAX:
                return new Token(OR_TOKEN, OR_SYNTAX, pos);
            case IF_SYNTAX:
                return new Token(IF_TOKEN, IF_SYNTAX, pos);
            case ELSE_SYNTAX:
                return new Token(ELSE_TOKEN, ELSE_SYNTAX, pos);
            case LOOP_SYNTAX:
                return new Token(LOOP_TOKEN, LOOP_SYNTAX, pos);
            case FROM_SYNTAX: // Pseudo for equals
                return new Token(EQUALS_TOKEN, FROM_SYNTAX, pos);
            case TO_SYNTAX:
                return new Token(TO_TOKEN, TO_SYNTAX, pos);
            default:
                return new Token(IDENTIFIER_TOKEN, text, text, pos);
        }
    }

    private Token stringToken()
    {
        StringBuilder syntaxBuilder = new StringBuilder(); // Includes quotes
        StringBuilder valueBuilder = new StringBuilder(); // Doesn't include quotes
        TokenType tokenType;

        syntaxBuilder.append(currentChar());
        int startPos = currentPositionThenNext(1);

        while (true)
        {
            String currentChar = currentChar();

            if (currentChar.equals("\0") || currentChar.equals("\r") || currentChar.equals("\n"))
            {
                tokenType = BAD_TOKEN;
                LexicalError error = incompleteString(syntaxBuilder.toString(), startPos, position - startPos);
                errorHandler.add(error);
                break;
            }
            else if (currentChar.equals("\""))
            {
                tokenType = STRING_TOKEN;
                syntaxBuilder.append(currentChar);
                nextPosition();
                break;
            }
            syntaxBuilder.append(currentChar);
            valueBuilder.append(currentChar);
            nextPosition();
        }

        return new Token(tokenType, syntaxBuilder.toString(), valueBuilder.toString(), startPos);
    }

    private Token commentToken()
    {
        StringBuilder syntaxBuilder = new StringBuilder();
        StringBuilder valueBuilder = new StringBuilder();

        syntaxBuilder.append(currentChar());
        int startPos = currentPositionThenNext(1);

        while (true)
        {
            String currentChar = currentChar();

            if (currentChar.equals("\0") || currentChar.equals("\r") || currentChar.equals("\n"))
                break;

            syntaxBuilder.append(currentChar);
            valueBuilder.append(currentChar);
            nextPosition();
        }

        return new Token(COMMENT_TOKEN, syntaxBuilder.toString(), valueBuilder.toString(), startPos);
    }

    private Token operatorToken()
    {
        String currentChar = currentChar();
        String nextChar = nextChar();

        switch (currentChar)
        {
            case HASH_SYNTAX:
                return commentToken();
            case DOUBLE_QUOTES_SYNTAX:
                return stringToken();
            case OPEN_BRACE_SYNTAX:
                return new Token(OPEN_BRACE_TOKEN, OPEN_BRACE_SYNTAX, currentPositionThenNext(1));
            case CLOSE_BRACE_SYNTAX:
                return new Token(CLOSE_BRACE_TOKEN, CLOSE_BRACE_SYNTAX, currentPositionThenNext(1));
            case OPEN_PARENTHESIS_SYNTAX:
                return new Token(OPEN_PARENTHESIS_TOKEN, OPEN_PARENTHESIS_SYNTAX, currentPositionThenNext(1));
            case CLOSE_PARENTHESIS_SYNTAX:
                return new Token(CLOSE_PARENTHESIS_TOKEN, CLOSE_PARENTHESIS_SYNTAX, currentPositionThenNext(1));
            case NOT_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(NOT_EQUALS_TOKEN, NOT_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(NOT_TOKEN, NOT_SYNTAX, currentPositionThenNext(1));
            case PLUS_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(PLUS_EQUALS_TOKEN, PLUS_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(PLUS_TOKEN, PLUS_SYNTAX, currentPositionThenNext(1));
            case MINUS_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(MINUS_EQUALS_TOKEN, MINUS_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(MINUS_TOKEN, MINUS_SYNTAX, currentPositionThenNext(1));
            case STAR_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(STAR_EQUALS_TOKEN, STAR_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(STAR_TOKEN, STAR_SYNTAX, currentPositionThenNext(1));
            case SLASH_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(SLASH_EQUALS_TOKEN, SLASH_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(SLASH_TOKEN, SLASH_SYNTAX, currentPositionThenNext(1));
            case PERCENT_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(PERCENT_EQUALS_TOKEN, PERCENT_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(PERCENT_TOKEN, PERCENT_SYNTAX, currentPositionThenNext(1));
            case CARET_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(CARET_EQUALS_TOKEN, CARET_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(CARET_TOKEN, CARET_SYNTAX, currentPositionThenNext(1));
            case GREATER_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(GREATER_EQUALS_TOKEN, GREATER_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(GREATER_TOKEN, GREATER_SYNTAX, currentPositionThenNext(1));
            case LESS_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(LESS_EQUALS_TOKEN, LESS_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(LESS_TOKEN, LESS_SYNTAX, currentPositionThenNext(1));
            case EQUALS_SYNTAX:
                if (EQUALS_SYNTAX.equals(nextChar))
                    return new Token(EQUALS_EQUALS_TOKEN, EQUALS_EQUALS_SYNTAX, currentPositionThenNext(2));
                return new Token(EQUALS_TOKEN, EQUALS_SYNTAX, currentPositionThenNext(1));
            default:
                return badToken();
        }
    }

    private Token badToken()
    {
        String syntax = sourceInput.substring(minimumZero(position - 1));
        LexicalError error = badCharacter(currentChar(), position, 1);
        errorHandler.add(error);

        return new Token(BAD_TOKEN, syntax, position, currentPositionThenNext(1));
    }

    private String currentChar()
    {
        return peek(0);
    }

    private String nextChar()
    {
        return peek(1);
    }

    private String peek(int offset)
    {
        int index = position + offset;
        if (index >= sourceInput.length() || index < 0)
            return EOF_SYNTAX;

        return Character.toString(sourceInput.charAt(index));
    }

    private void nextPosition()
    {
        position++;
    }

    private int currentPositionThenNext(int increment)
    {
        int currentPos = position;
        position += increment;
        return currentPos;
    }

    private static boolean isLineBreak(String str)
    {
        return str.equals(LINE_BREAK_SYNTAX) || str.equals("\r") || str.equals("\n");
    }

    private static boolean isWhitespace(String str)
    {
        return str.isBlank();
    }

    private static boolean isDigit(String str)
    {
        if (str.length() != 1)
            return false;
        return Character.isDigit(str.charAt(0));
    }

    private static boolean isLetter(String str)
    {
        if (str.length() != 1)
            return false;
        return Character.isLetter(str.charAt(0)) || str.equals("_");
    }

    private static boolean isIntegerParsable(String str)
    {
        try
        {
            Integer.parseInt(str);
            return true;
        }
        catch (NumberFormatException error)
        {
            return false;
        }
    }

    private static boolean isDoubleParsable(String str)
    {
        try
        {
            Double.parseDouble(str);
            return true;
        }
        catch (NumberFormatException error)
        {
            return false;
        }
    }

    private static int minimumZero(int num)
    {
        return Math.max(num, 0);
    }
}
