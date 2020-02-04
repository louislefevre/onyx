package main.java.analysis;

import java.util.List;

public abstract class Node
{
    public abstract TokenType getType();

    public abstract List<Node> getChildren();
}
