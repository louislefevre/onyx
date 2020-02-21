package analysis.lexical;

import identifiers.TokenType;

import java.util.List;

public abstract class Node
{
    public abstract TokenType getTokenType();

    public abstract List<Node> getChildren();
}
