package analysis.syntax;

import identifiers.TokenType;

import java.util.List;

public abstract class Expression
{
    public abstract TokenType getTokenType();
    public abstract List<Object> getChildren();
}
