package analysis.syntax;

import analysis.lexical.Token;
import identifiers.TokenType;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;

import static identifiers.TokenType.*;

public class ExpressionBinder
{
    private final static HashMap<TokenType, TokenType[]> binds = new HashMap<>();

    static
    {
        binds.put(NUMBER_TOKEN, new TokenType[]{PLUS_TOKEN, MINUS_TOKEN, STAR_TOKEN, SLASH_TOKEN, CARET_TOKEN,
                                                PERCENT_TOKEN, CLOSE_PARENTHESIS_TOKEN, NOT_EQUALS_TOKEN,
                                                EQUALS_EQUALS_TOKEN, GREATER_TOKEN, LESS_TOKEN, GREATER_EQUALS_TOKEN,
                                                LESS_EQUALS_TOKEN});

        binds.put(PLUS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(MINUS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(STAR_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(SLASH_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(CARET_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(PERCENT_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(GREATER_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(LESS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(GREATER_EQUALS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});
        binds.put(LESS_EQUALS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN});

        binds.put(OPEN_PARENTHESIS_TOKEN, new TokenType[]{NUMBER_TOKEN, CLOSE_PARENTHESIS_TOKEN,
                                                          NOT_TOKEN, FALSE_KEYWORD_TOKEN, TRUE_KEYWORD_TOKEN,
                                                          IDENTIFIER_KEYWORD_TOKEN});
        binds.put(CLOSE_PARENTHESIS_TOKEN, new TokenType[]{PLUS_TOKEN, MINUS_TOKEN, STAR_TOKEN, SLASH_TOKEN,
                                                           CARET_TOKEN, PERCENT_TOKEN, NOT_EQUALS_TOKEN,
                                                           EQUALS_EQUALS_TOKEN, GREATER_TOKEN, LESS_TOKEN,
                                                           GREATER_EQUALS_TOKEN, LESS_EQUALS_TOKEN, AND_TOKEN,
                                                           OR_TOKEN});

        binds.put(NOT_TOKEN, new TokenType[]{TRUE_KEYWORD_TOKEN, FALSE_KEYWORD_TOKEN, IDENTIFIER_KEYWORD_TOKEN});
        binds.put(EQUALS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN, TRUE_KEYWORD_TOKEN,
                                                FALSE_KEYWORD_TOKEN, IDENTIFIER_KEYWORD_TOKEN});
        binds.put(EQUALS_EQUALS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN, TRUE_KEYWORD_TOKEN,
                                                       FALSE_KEYWORD_TOKEN, IDENTIFIER_KEYWORD_TOKEN});
        binds.put(NOT_EQUALS_TOKEN, new TokenType[]{NUMBER_TOKEN, OPEN_PARENTHESIS_TOKEN, TRUE_KEYWORD_TOKEN,
                                                    FALSE_KEYWORD_TOKEN, IDENTIFIER_KEYWORD_TOKEN});

        binds.put(TRUE_KEYWORD_TOKEN, new TokenType[]{AND_TOKEN, OR_TOKEN, EQUALS_EQUALS_TOKEN,});
        binds.put(FALSE_KEYWORD_TOKEN, new TokenType[]{AND_TOKEN, OR_TOKEN, EQUALS_TOKEN});
        binds.put(AND_TOKEN, new TokenType[]{TRUE_KEYWORD_TOKEN, FALSE_KEYWORD_TOKEN, IDENTIFIER_KEYWORD_TOKEN});
        binds.put(OR_TOKEN, new TokenType[]{TRUE_KEYWORD_TOKEN, FALSE_KEYWORD_TOKEN, IDENTIFIER_KEYWORD_TOKEN});

        binds.put(IDENTIFIER_KEYWORD_TOKEN, new TokenType[]{PLUS_TOKEN, MINUS_TOKEN, STAR_TOKEN, SLASH_TOKEN,
                                                            CARET_TOKEN, PERCENT_TOKEN, NOT_EQUALS_TOKEN, EQUALS_TOKEN,
                                                            EQUALS_EQUALS_TOKEN, GREATER_TOKEN, LESS_TOKEN,
                                                            GREATER_EQUALS_TOKEN, LESS_EQUALS_TOKEN,
                                                            CLOSE_PARENTHESIS_TOKEN, AND_TOKEN, OR_TOKEN});
    }

    public static boolean isBindable(@NotNull Token current, Token next)
    {
        TokenType tokenType = current.getTokenType();

        if (!binds.containsKey(tokenType))
            return false;

        TokenType[] typeList = binds.get(tokenType);

        for (TokenType type : typeList)
            if (next.getTokenType() == type)
                return true;

        return false;
    }
}
