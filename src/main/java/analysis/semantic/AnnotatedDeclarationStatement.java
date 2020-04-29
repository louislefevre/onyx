package analysis.semantic;

import identifiers.AnnotatedStatementType;
import lombok.Getter;
import symbols.Symbol;

@Getter
public class AnnotatedDeclarationStatement implements AnnotatedStatement
{
    private final Symbol symbol;
    private final AnnotatedExpression initializerExpression;
    private final AnnotatedStatementType annotatedStatementType;

    public AnnotatedDeclarationStatement(Symbol symbol, AnnotatedExpression initializerExpression)
    {
        this.symbol = symbol;
        this.initializerExpression = initializerExpression;
        this.annotatedStatementType = AnnotatedStatementType.ANNOTATED_DECLARATION_STATEMENT;
    }
}
