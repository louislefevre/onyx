package analysis.semantic;

import analysis.syntax.*;
import errors.ErrorHandler;
import errors.SemanticError;
import identifiers.ObjectType;
import org.jetbrains.annotations.Nullable;
import symbols.SymbolTable;

public final class TypeChecker
{
    private final ParseTree parseTree;
    private final ErrorHandler errorHandler;
    private final SymbolTable symbolTable;

    public TypeChecker(Parser parser, ErrorHandler errorHandler, SymbolTable symbolTable)
    {
        this.parseTree = parser.getParseTree();
        this.errorHandler = errorHandler;
        this.symbolTable = symbolTable;
    }

    public AnnotatedParseTree getAnnotatedParseTree()
    {
        return new AnnotatedParseTree(this.getAnnotatedExpression());
    }

    private AnnotatedExpression getAnnotatedExpression()
    {
        return this.annotate(this.parseTree.getExpression());
    }

    @Nullable
    private AnnotatedExpression annotate(Expression syntax)
    {
        return this.annotateExpression(syntax);
    }

    private AnnotatedExpression annotateExpression(Expression syntax)
    {
        switch (syntax.getExpressionType())
        {
            case PARENTHESIZED_EXPRESSION_TOKEN:
                return this.annotateParenthesizedExpression((ParenthesizedExpression) syntax);
            case LITERAL_EXPRESSION_TOKEN:
                return this.annotateLiteralExpression((LiteralExpression) syntax);
            case UNARY_EXPRESSION_TOKEN:
                return this.annotateUnaryExpression((UnaryExpression) syntax);
            case BINARY_EXPRESSION_TOKEN:
                return this.annotateBinaryExpression((BinaryExpression) syntax);
            case NAME_EXPRESSION_TOKEN:
                return this.annotateNameExpression((NameExpression) syntax);
            case ASSIGNMENT_EXPRESSION_TOKEN:
                return this.annotateAssignmentExpression((AssignmentExpression) syntax);
            default:
                return this.unknownExpression(syntax);
        }
    }

    private AnnotatedExpression annotateParenthesizedExpression(ParenthesizedExpression syntax)
    {
        return this.annotateExpression(syntax.getExpression());
    }

    private AnnotatedExpression annotateLiteralExpression(LiteralExpression syntax)
    {
        Object value = syntax.getValue();
        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression syntax)
    {
        AnnotatedExpression annotatedOperand = this.annotateExpression(syntax.getOperand());
        AnnotatedUnaryOperator annotatedOperator =
                TypeBinder.bindUnaryOperators(syntax.getOperatorToken().getTokenType(),
                                              annotatedOperand.getObjectType());

        if (annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedUnaryOperator(syntax.getOperatorToken().getSpan(),
                                                                            syntax.getOperatorToken().getSyntax(),
                                                                            annotatedOperand.getObjectType()));
            return annotatedOperand;
        }

        return new AnnotatedUnaryExpression(annotatedOperator, annotatedOperand);
    }

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression syntax)
    {
        AnnotatedExpression annotatedLeft = this.annotateExpression(syntax.getLeftTerm());
        AnnotatedExpression annotatedRight = this.annotateExpression(syntax.getRightTerm());
        AnnotatedBinaryOperator annotatedOperator =
                TypeBinder.bindBinaryOperators(syntax.getOperatorToken().getTokenType(),
                                               annotatedLeft.getObjectType(),
                                               annotatedRight.getObjectType());

        if (annotatedOperator == null)
        {
            this.errorHandler.addError(SemanticError.undefinedBinaryOperator(syntax.getOperatorToken().getSpan(),
                                                                             syntax.getOperatorToken().getSyntax(),
                                                                             annotatedLeft.getObjectType(),
                                                                             annotatedRight.getObjectType()));
            return annotatedLeft;
        }

        return new AnnotatedBinaryExpression(annotatedLeft, annotatedOperator, annotatedRight);
    }

    private AnnotatedExpression annotateNameExpression(NameExpression syntax)
    {
        String name = syntax.getIdentifierToken().getSyntax();

        if (!this.symbolTable.containsSymbol(name))
        {
            this.errorHandler.addError(SemanticError.undefinedName(syntax.getIdentifierToken().getSpan(), name));
            return new AnnotatedLiteralExpression(null);
        }
        ObjectType type = this.symbolTable.getSymbol(name).getType();

        return new AnnotatedVariableExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression syntax)
    {
        String name = syntax.getIdentifierToken().getSyntax();
        AnnotatedExpression expression = this.annotateExpression(syntax.getExpression());
        return new AnnotatedAssignmentExpression(name, expression);
    }

    private AnnotatedExpression unknownExpression(Expression syntax)
    {
        try
        {
            throw SemanticError.undefinedExpression(syntax.getExpressionType().toString());
        } catch (Exception err)
        {
            System.out.println(err.getMessage());
        }
        return null;
    }
}
