package analysis.syntax;

import identifiers.StatementType;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Getter
public final class ExpressionStatement extends Statement
{
    private final Expression expression;
    private final StatementType statementType;
    private final List<Object> children;

    public ExpressionStatement(Expression expression)
    {
        this.expression = expression;
        this.statementType = StatementType.EXPRESSION_STATEMENT;
        this.children = new ArrayList<>(Collections.singletonList(expression));
    }
}
