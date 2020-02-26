package analysis.semantic;

import analysis.syntax.*;
import errors.ErrorHandler;

public final class TypeChecker
{
    private final ParseTree parseTree;

    public TypeChecker(Parser parser)
    {
        this.parseTree = parser.getParseTree();
    }

    public AnnotatedParseTree getAnnotatedParseTree()
    {
        return new AnnotatedParseTree(this.getAnnotatedExpression());
    }

    private AnnotatedExpression getAnnotatedExpression()
    {
        return this.annotate(this.parseTree.getExpression());
    }

    private AnnotatedExpression annotate(Expression syntax)
    {
        try
        {
            return this.annotateExpression(syntax);
        }
        catch(Exception error)
        {
            System.out.println(error.getMessage());
            return null;
        }
    }

    private AnnotatedExpression annotateExpression(Expression syntax) throws Exception
    {
        switch(syntax.getTokenType())
        {
            case PARENTHESIZED_EXPRESSION_TOKEN:
                return this.annotateParenthesizedExpression((ParenthesizedExpression)syntax);
            case LITERAL_EXPRESSION_TOKEN:
                return this.annotateLiteralExpression((LiteralExpression)syntax);
            case UNARY_EXPRESSION_TOKEN:
                return this.annotateUnaryExpression((UnaryExpression)syntax);
            case BINARY_EXPRESSION_TOKEN:
                return this.annotateBinaryExpression((BinaryExpression)syntax);
            default:
                throw new Exception(String.format("Unexpected syntax '%s'", syntax.getTokenType()));
        }
    }

    private AnnotatedExpression annotateParenthesizedExpression(ParenthesizedExpression syntax) throws Exception
    {
        return this.annotateExpression(syntax.getExpression());
    }

    private AnnotatedExpression annotateLiteralExpression(LiteralExpression syntax)
    {
        Object value = syntax.getValue();
        if(value == null)
            value = 0;

        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression syntax) throws Exception
    {
        AnnotatedExpression annotatedOperand = this.annotateExpression(syntax.getOperand());
        AnnotatedUnaryOperator annotatedOperator = TypeBinder.bindUnaryOperators(syntax.getOperatorToken().getTokenType(), annotatedOperand.getObjectType());

        if(annotatedOperator == null)
        {
            ErrorHandler.addSemanticError(String.format("Unary operator '%1s' is not defined for type '%2s'.", syntax.getOperatorToken().getSyntax(), annotatedOperand.getObjectType()));
            return annotatedOperand;
        }

        return new AnnotatedUnaryExpression(annotatedOperator, annotatedOperand);
    }

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression syntax) throws Exception
    {
        AnnotatedExpression annotatedLeft = this.annotateExpression(syntax.getLeftTerm());
        AnnotatedExpression annotatedRight = this.annotateExpression(syntax.getRightTerm());
        AnnotatedBinaryOperator annotatedOperator = TypeBinder.bindBinaryOperators(syntax.getOperatorToken().getTokenType(), annotatedLeft.getObjectType(), annotatedRight.getObjectType());

        if(annotatedOperator == null)
        {
            ErrorHandler.addSemanticError(String.format("Binary operator '%1s' is not defined for type '%2s' and '%3s'.", syntax.getOperatorToken().getSyntax(), annotatedLeft.getObjectType(), annotatedRight.getObjectType()));
            return annotatedLeft;
        }

        return new AnnotatedBinaryExpression(annotatedLeft, annotatedOperator, annotatedRight);
    }
}