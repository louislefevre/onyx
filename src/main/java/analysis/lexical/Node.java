package analysis.lexical;

import analysis.identifiers.TokenType;

import java.util.List;

public abstract class Node
{
    public abstract TokenType getTokenType();

    public abstract List<Node> getChildren();
}
