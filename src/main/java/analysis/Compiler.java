package main.java.analysis;

import main.java.analysis.lexical.Lexer;
import main.java.analysis.syntactic.Evaluator;
import main.java.analysis.syntactic.Parser;
import main.java.analysis.syntactic.SyntaxTree;

public class Compiler
{
    private final String input;

    public Compiler(String input)
    {
        this.input = input;
    }

    public int compile()
    {
        Lexer lexer = new Lexer(this.input);
        Parser parser = new Parser(lexer);
        SyntaxTree syntaxTree = new SyntaxTree(parser);
        Evaluator evaluator = new Evaluator(syntaxTree);

        syntaxTree.showTree(); // Testing purposes

        return evaluator.evaluate();
    }
}
