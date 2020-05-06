package analysis.lexical;

import errors.ErrorHandler;
import errors.LexicalError;
import identifiers.TokenType;
import lombok.Getter;
import source.SourceInput;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

import static analysis.lexical.Syntax.*;
import static errors.LexicalError.badCharacter;
import static errors.LexicalError.incompleteString;
import static errors.LexicalError.invalidDouble;
import static errors.LexicalError.invalidInt;
import static identifiers.TokenType.*;

public final class Lexer
{
    private final String sourceText;
    @Getter
    private final ErrorHandler errorHandler;
    @Getter
    private final SymbolTable symbolTable;
    @Getter
    private final boolean replMode;
    private int position;

    public Lexer(SourceInput sourceInput)
    {
        this.sourceText = sourceInput.getSourceText();
        this.errorHandler = sourceInput.getErrorHandler();
        this.symbolTable = sourceInput.getSymbolTable();
        this.replMode = sourceInput.isReplMode();
        this.position = 0;
    }

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
        if (position >= sourceText.length())
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
        return new Token(LINE_BREAK_TOKEN, LINE_BREAK_SYNTAX, currentPositionThenNext(1));
    }

    private Token whitespaceToken()
    {
        int startPos = position;

        while (isWhitespace(currentChar()))
            nextPosition();

        String syntax = sourceText.substring(startPos, position);

        return new Token(WHITE_SPACE_TOKEN, syntax, startPos);
    }

    private Token integerToken()
    {
        int startPos = position;

        while (isDigit(currentChar()))
            currentPositionThenNext(1);

        if (currentChar().equals(DECIMAL_POINT_SYNTAX))
            return doubleToken(startPos);

        String syntax = sourceText.substring(startPos, position);
        int value = 0;

        if (isIntegerParsable(syntax))
        {
            value = Integer.parseInt(syntax);
        }
        else
        {
            LexicalError error = invalidInt(syntax, startPos, position - startPos);
            errorHandler.addError(error);
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

        String syntax = sourceText.substring(startPos, position);
        double value = 0;

        if (isDoubleParsable(syntax))
        {
            value = Double.parseDouble(syntax);
        }
        else
        {
            LexicalError error = invalidDouble(syntax, startPos, position - startPos);
            errorHandler.addError(error);
        }

        return new Token(DOUBLE_TOKEN, syntax, value, startPos);
    }

    private Token letterToken()
    {
        int startPos = position;

        while (isLetter(currentChar()))
            nextPosition();

        String syntax = sourceText.substring(startPos, position).toLowerCase();

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
                errorHandler.addError(error);
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
        String syntax = sourceText.substring(minimumZero(position - 1));
        LexicalError error = badCharacter(currentChar(), position, 1);
        errorHandler.addError(error);

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
        if (index >= sourceText.length() || index < 0)
            return EOF_SYNTAX;

        return Character.toString(sourceText.charAt(index));
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
        return str.equals(LINE_BREAK_SYNTAX);
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
