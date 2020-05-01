package analysis.lexical;

import errors.ErrorHandler;
import errors.LexicalError;
import identifiers.TokenType;
import source.SourceInput;
import symbols.SymbolTable;

import java.util.ArrayList;
import java.util.List;

public final class Lexer
{
    private final SourceInput sourceInput;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;
    private int position;

    public Lexer(SourceInput sourceInput, ErrorHandler errorHandler, SymbolTable symbolTable)
    {
        this.sourceInput = sourceInput;
        this.errorHandler = errorHandler;
        this.symbolTable = symbolTable;
        this.position = 0;
    }

    public ErrorHandler getErrorHandler()
    {
        return this.errorHandler;
    }

    public SymbolTable getSymbolTable()
    {
        return this.symbolTable;
    }

    public List<Token> getTokens()
    {
        return this.lexTokens();
    }

    private List<Token> lexTokens()
    {
        List<Token> tokens = new ArrayList<>();
        Token token;
        do
        {
            token = this.nextToken();
            if (token.getTokenType() != TokenType.BAD_TOKEN &&
                token.getTokenType() != TokenType.WHITE_SPACE_TOKEN &&
                token.getTokenType() != TokenType.COMMENT_TOKEN)
                tokens.add(token);
        } while (token.getTokenType() != TokenType.EOF_TOKEN);
        return tokens;
    }

    private Token nextToken()
    {
        if (this.position >= this.sourceInput.length())
            return this.endToken();
        else if (isWhitespace(this.currentChar()))
            return this.whitespaceToken();
        else if (isDigit(this.currentChar()))
            return this.integerToken();
        else if (isLetter(this.currentChar()))
            return this.letterToken();
        return this.symbolToken();
    }

    private Token endToken()
    {
        return new Token(TokenType.EOF_TOKEN, Syntax.EOF.getSyntax(), this.position);
    }

    private Token whitespaceToken()
    {
        int startPos = this.position;

        while (isWhitespace(this.currentChar()))
            this.nextPosition();

        String syntax = this.sourceInput.substring(startPos, this.position);

        return new Token(TokenType.WHITE_SPACE_TOKEN, syntax, startPos);
    }

    private Token integerToken()
    {
        int startPos = this.position;

        while (isDigit(this.currentChar()))
            this.currentPositionThenNext(1);

        if (this.currentChar().equals(Syntax.DECIMAL_POINT.getSyntax()))
            return this.doubleToken(startPos);

        String syntax = this.sourceInput.substring(startPos, this.position);
        int value = 0;

        if (isIntegerParsable(syntax))
            value = Integer.parseInt(syntax);
        else
            this.errorHandler.addError(LexicalError.invalidInt(syntax, startPos, this.position - startPos));

        return new Token(TokenType.INTEGER_TOKEN, syntax, value, startPos);
    }

    private Token doubleToken(int startPos)
    {
        do // 'do' to skip the decimal point
        {
            this.currentPositionThenNext(1);
        }
        while (isDigit(this.currentChar()));

        String syntax = this.sourceInput.substring(startPos, this.position);
        double value = 0;

        if (isDoubleParsable(syntax))
            value = Double.parseDouble(syntax);
        else
            this.errorHandler.addError(LexicalError.invalidDouble(syntax, startPos, this.position - startPos));

        return new Token(TokenType.DOUBLE_TOKEN, syntax, value, startPos);
    }

    private Token letterToken()
    {
        int startPos = this.position;

        while (isLetter(this.currentChar()))
            this.nextPosition();

        String syntax = this.sourceInput.substring(startPos, this.position);

        return getKeywordToken(syntax, startPos);
    }

    private static Token getKeywordToken(String text, int pos)
    {
        if (Syntax.TRUE.getSyntax().equals(text))
            return new Token(TokenType.TRUE_KEYWORD_TOKEN, Syntax.TRUE.getSyntax(), true, pos);
        else if (Syntax.FALSE.getSyntax().equals(text))
            return new Token(TokenType.FALSE_KEYWORD_TOKEN, Syntax.FALSE.getSyntax(), false, pos);
        else if (Syntax.AND.getSyntax().equals(text))
            return new Token(TokenType.AND_TOKEN, Syntax.AND.getSyntax(), pos);
        else if (Syntax.OR.getSyntax().equals(text))
            return new Token(TokenType.OR_TOKEN, Syntax.OR.getSyntax(), pos);

        return new Token(TokenType.IDENTIFIER_TOKEN, text, text, pos);
    }

    private Token stringToken()
    {
        StringBuilder syntaxBuilder = new StringBuilder(); // Includes quotes
        StringBuilder valueBuilder = new StringBuilder(); // Doesn't include quotes
        TokenType tokenType;

        syntaxBuilder.append(this.currentChar());
        int startPos = this.currentPositionThenNext(1);

        while (true)
        {
            String currentChar = this.currentChar();

            if (currentChar.equals("\0") || currentChar.equals("\r") || currentChar.equals("\n"))
            {
                tokenType = TokenType.BAD_TOKEN;
                this.errorHandler.addError(LexicalError.incompleteString(valueBuilder.toString(), startPos,
                                                                         this.position - startPos));
                break;
            }
            else if (currentChar.equals("\""))
            {
                tokenType = TokenType.STRING_TOKEN;
                syntaxBuilder.append(currentChar);
                this.nextPosition();
                break;
            }
            syntaxBuilder.append(currentChar);
            valueBuilder.append(currentChar);
            this.nextPosition();
        }

        return new Token(tokenType, syntaxBuilder.toString(), valueBuilder.toString(), startPos);
    }

    private Token commentToken()
    {
        StringBuilder syntaxBuilder = new StringBuilder();
        StringBuilder valueBuilder = new StringBuilder();

        syntaxBuilder.append(this.currentChar());
        int startPos = this.currentPositionThenNext(1);

        while (true)
        {
            String currentChar = this.currentChar();

            if (currentChar.equals("\0") || currentChar.equals("\r") || currentChar.equals("\n"))
                break;

            syntaxBuilder.append(currentChar);
            valueBuilder.append(currentChar);
            this.nextPosition();
        }

        return new Token(TokenType.COMMENT_TOKEN, syntaxBuilder.toString(), valueBuilder.toString(), startPos);
    }

    private Token symbolToken()
    {
        String currentChar = this.currentChar();
        String nextChar = this.nextChar();

        if (Syntax.HASH.getSyntax().equals(currentChar))
        {
            return this.commentToken();
        }
        else if (Syntax.DOUBLE_QUOTES.getSyntax().equals(currentChar))
        {
            return this.stringToken();
        }
        else if (Syntax.PLUS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.PLUS_TOKEN, Syntax.PLUS.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.MINUS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.MINUS_TOKEN, Syntax.MINUS.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.STAR.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.STAR_TOKEN, Syntax.STAR.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.SLASH.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.SLASH_TOKEN, Syntax.SLASH.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.CARET.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.CARET_TOKEN, Syntax.CARET.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.PERCENT.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.PERCENT_TOKEN, Syntax.PERCENT.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.OPEN_BRACE.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.OPEN_BRACE_TOKEN, Syntax.OPEN_BRACE.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.CLOSE_BRACE.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.CLOSE_BRACE_TOKEN, Syntax.CLOSE_BRACE.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.OPEN_PARENTHESIS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.OPEN_PARENTHESIS_TOKEN, Syntax.OPEN_PARENTHESIS.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.CLOSE_PARENTHESIS.getSyntax().equals(currentChar))
        {
            return new Token(TokenType.CLOSE_PARENTHESIS_TOKEN, Syntax.CLOSE_PARENTHESIS.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.EQUALS.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.EQUALS_EQUALS_TOKEN, Syntax.EQUALS_EQUALS.getSyntax(),
                                 this.currentPositionThenNext(2));
            return new Token(TokenType.EQUALS_TOKEN, Syntax.EQUALS.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.NOT.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.NOT_EQUALS_TOKEN, Syntax.NOT_EQUALS.getSyntax(),
                                 this.currentPositionThenNext(2));
            return new Token(TokenType.NOT_TOKEN, Syntax.NOT.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.GREATER.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.GREATER_EQUALS_TOKEN, Syntax.GREATER_EQUALS.getSyntax(),
                                 this.currentPositionThenNext(2));
            return new Token(TokenType.GREATER_TOKEN, Syntax.GREATER.getSyntax(),
                             this.currentPositionThenNext(1));
        }
        else if (Syntax.LESS.getSyntax().equals(currentChar))
        {
            if (Syntax.EQUALS.getSyntax().equals(nextChar))
                return new Token(TokenType.LESS_EQUALS_TOKEN, Syntax.LESS_EQUALS.getSyntax(),
                                 this.currentPositionThenNext(2));
            return new Token(TokenType.LESS_TOKEN, Syntax.LESS.getSyntax(),
                             this.currentPositionThenNext(1));
        }

        return this.badToken();
    }

    private Token badToken()
    {
        this.errorHandler.addError(LexicalError.badCharacter(this.currentChar(), this.position, 1));
        return new Token(TokenType.BAD_TOKEN,
                         sourceInput.substring(minimumZero(this.position - 1), this.position),
                         this.currentPositionThenNext(1));
    }

    private String currentChar()
    {
        return this.peek(0);
    }

    private String nextChar()
    {
        return this.peek(1);
    }

    private String peek(int offset)
    {
        int index = this.position + offset;

        if (index >= this.sourceInput.length() || index < 0)
            return Syntax.EOF.getSyntax();
        return Character.toString(this.sourceInput.charAt(index));
    }

    private void nextPosition()
    {
        this.position++;
    }

    private int currentPositionThenNext(int increment)
    {
        int currentPos = this.position;
        this.position += increment;
        return currentPos;
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
        return Character.isLetter(str.charAt(0)) ||
               str.equals("_") || str.equals("-");
    }

    private static boolean isIntegerParsable(String str)
    {
        try
        {
            Integer.parseInt(str);
            return true;
        } catch (NumberFormatException error)
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
        } catch (NumberFormatException error)
        {
            return false;
        }
    }

    private static int minimumZero(int num)
    {
        return Math.max(num, 0);
    }
}
