package main.java.codeanalysis;

import java.util.ArrayList;
import java.util.List;

public class Parser
{
    private Lexer lexer;
    private SyntaxToken token;
    private List<SyntaxToken> tokens;
    private int position;

    public Parser(String text)
    {
        this.lexer = new Lexer(text);
        this.tokens = new ArrayList<>();

        do
        {
            this.token = lexer.nextToken();

            if(this.token.getKind() != SyntaxKind.WhiteSpace && this.token.getKind() != SyntaxKind.BadToken)
            {
                this.tokens.add(this.token);
            }
        } while(this.token.getKind() != SyntaxKind.EndOfFileToken);
    }

    private SyntaxToken peek(int offset)
    {
        int index = this.position + offset;
        if(index >= this.tokens.size())
            return this.tokens.get(this.tokens.size() - 1);
        return this.tokens.get(index);
    }

    private SyntaxToken current() { return peek(0); }

    private SyntaxToken nextToken()
    {
        SyntaxToken current = this.current();
        this.position++;
        return current;
    }

    private SyntaxToken match(SyntaxKind kind)
    {
        if(this.current().getKind() == kind)
            return this.nextToken();
        return new SyntaxToken(kind, this.current().getPosition(), null, null);
    }

    public ExpressionSyntax parse()
    {
        ExpressionSyntax left = parsePrimaryExpression();

        while(this.current().getKind() == SyntaxKind.PlusToken || this.current().getKind() == SyntaxKind.MinusToken)
        {
            SyntaxToken operatorToken = this.nextToken();
            ExpressionSyntax right = parsePrimaryExpression();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax parsePrimaryExpression()
    {
        SyntaxToken numberToken = this.match(SyntaxKind.NumberToken);
        return new NumberExpressionSyntax(numberToken);
    }
}
