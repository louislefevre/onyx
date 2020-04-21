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
        try
        {
            return this.annotateExpression(syntax);
        } catch (Exception error)
        {
            System.out.println(error.getMessage());
            return null;
        }
    }

    private AnnotatedExpression annotateExpression(Expression syntax) throws Exception
    {
        switch (syntax.getTokenType())
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
        return new AnnotatedLiteralExpression(value);
    }

    private AnnotatedExpression annotateUnaryExpression(UnaryExpression syntax) throws Exception
    {
        AnnotatedExpression annotatedOperand = this.annotateExpression(syntax.getOperand());
        AnnotatedUnaryOperator annotatedOperator =
                TypeBinder.bindUnaryOperators(syntax.getOperatorToken().getTokenType(),
                                              annotatedOperand.getObjectType());

        if (annotatedOperator == null)
        {
            SemanticError error = SemanticError.undefinedUnaryOperator(syntax.getOperatorToken().getSpan(),
                                                                       syntax.getOperatorToken().getSyntax(),
                                                                       annotatedOperand.getObjectType());
            this.errorHandler.addError(error);
            return annotatedOperand;
        }

        return new AnnotatedUnaryExpression(annotatedOperator, annotatedOperand);
    }

    private AnnotatedExpression annotateBinaryExpression(BinaryExpression syntax) throws Exception
    {
        AnnotatedExpression annotatedLeft = this.annotateExpression(syntax.getLeftTerm());
        AnnotatedExpression annotatedRight = this.annotateExpression(syntax.getRightTerm());
        AnnotatedBinaryOperator annotatedOperator =
                TypeBinder.bindBinaryOperators(syntax.getOperatorToken().getTokenType(),
                                               annotatedLeft.getObjectType(),
                                               annotatedRight.getObjectType());

        if (annotatedOperator == null)
        {
            SemanticError error = SemanticError.undefinedBinaryOperator(syntax.getOperatorToken().getSpan(),
                                                                        syntax.getOperatorToken().getSyntax(),
                                                                        annotatedLeft.getObjectType(),
                                                                        annotatedRight.getObjectType());
            this.errorHandler.addError(error);
            return annotatedLeft;
        }

        return new AnnotatedBinaryExpression(annotatedLeft, annotatedOperator, annotatedRight);
    }

    private AnnotatedExpression annotateNameExpression(NameExpression syntax)
    {
        String name = syntax.getIdentifierToken().getSyntax();

        if (!this.symbolTable.containsSymbol(name))
        {
            SemanticError error = SemanticError.undefinedName(syntax.getIdentifierToken().getSpan(), name);
            this.errorHandler.addError(error);
            return new AnnotatedLiteralExpression(null);
        }
        ObjectType type = this.symbolTable.getSymbol(name).getType();

        return new AnnotatedVariableExpression(name, type);
    }

    private AnnotatedExpression annotateAssignmentExpression(AssignmentExpression syntax) throws Exception
    {
        String name = syntax.getIdentifierToken().getSyntax();
        AnnotatedExpression expression = this.annotateExpression(syntax.getExpression());
        return new AnnotatedAssignmentExpression(name, expression);
    }
}
