package analysis.syntax;

import analysis.lexical.Token;
import identifiers.StatementType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
public final class DeclarationStatement implements Statement
{
    private final Token varToken;
    private final Token identifierToken;
    private final Token equalsToken;
    private final Expression initializerExpression;
    private final StatementType statementType;
    private final List<Object> children;

    public DeclarationStatement(Token varToken, Token identifierToken, Token equalsToken,
                                Expression initializerExpression)
    {
        this.varToken = varToken;
        this.identifierToken = identifierToken;
        this.equalsToken = equalsToken;
        this.initializerExpression = initializerExpression;
        this.statementType = StatementType.DECLARATION_STATEMENT;
        this.children = new ArrayList<>(Arrays.asList(varToken, identifierToken, equalsToken, initializerExpression));
    }
}
