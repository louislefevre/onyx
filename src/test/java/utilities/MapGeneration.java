package utilities;

import analysis.lexical.Syntax;
import errors.Error;
import errors.ErrorHandler;
import errors.LexicalError;
import errors.SemanticError;
import errors.SyntaxError;
import identifiers.ObjectType;
import identifiers.TokenType;
import source.SourceSpan;

import java.util.HashMap;

class MapGeneration
{
    private MapGeneration() {}

    static HashMap<String, Object> integerCollection()
    {
        HashMap<String, Object> integers = new HashMap<>();
        integers.put("0", 0);     integers.put("1", 1);
        integers.put("100", 100); integers.put("12345", 12345);
        integers.put("2147483647", 2147483647);

        return integers;
    }

    static HashMap<String, Object> doubleCollection()
    {
        HashMap<String, Object> doubles = new HashMap<>();
        doubles.put("0.0", 0.0);     doubles.put("1.0", 1.0);
        doubles.put("100.0", 100.0); doubles.put("100.", 100.0);
        doubles.put("12.5", 12.5);   doubles.put("9.9", 9.9);

        return doubles;
    }

    static HashMap<String, Object> booleanCollection()
    {
        HashMap<String, Object> booleans = new HashMap<>();
        booleans.put("true", true);
        booleans.put("false", false);

        return booleans;
    }

    static HashMap<String, Object> stringCollection()
    {
        HashMap<String, Object> strings = new HashMap<>();
        strings.put("\"a\"", "a");
        strings.put("\"aaa\"", "aaa");
        strings.put("\"string\"", "string");
        strings.put("\"separated string\"", "separated string");

        return strings;
    }

    static HashMap<String, Object> unaryIntegerCollection()
    {
        HashMap<String, Object> integerUnarys = new HashMap<>();
        integerUnarys.put("+0", 0);                   integerUnarys.put("-0", 0);
        integerUnarys.put("+1", 1);                   integerUnarys.put("-1", -1);
        integerUnarys.put("+12345", 12345);           integerUnarys.put("-12345", -12345);
        integerUnarys.put("+2147483647", 2147483647); integerUnarys.put("-2147483647", -2147483647);

        return integerUnarys;
    }

    static HashMap<String, Object> unaryDoubleCollection()
    {
        HashMap<String, Object> doubleUnarys = new HashMap<>();
        doubleUnarys.put("+0.0", 0.0);                   doubleUnarys.put("-0.0", -0.0);
        doubleUnarys.put("+1.0", 1.0);                   doubleUnarys.put("-1.0", -1.0);
        doubleUnarys.put("+12345.0", 12345.0);           doubleUnarys.put("-12345.0", -12345.0);
        doubleUnarys.put("+2147483647.0", 2147483647.0); doubleUnarys.put("-2147483647.0", -2147483647.0);

        return doubleUnarys;
    }

    static HashMap<String, Object> unaryBooleanCollection()
    {
        HashMap<String, Object> booleanUnarys = new HashMap<>();
        booleanUnarys.put("!true", false);
        booleanUnarys.put("!false", true);

        return booleanUnarys;
    }

    static HashMap<String, Object> binaryIntegerCollection()
    {
        HashMap<String, Object> integerBinaries = new HashMap<>();
        integerBinaries.put("0 + 0", 0);      integerBinaries.put("0 - 0", 0);
        integerBinaries.put("0 * 0", 0);      integerBinaries.put("0 / 0", 0);
        integerBinaries.put("0 % 0", 0);      integerBinaries.put("0 ^ 0", 1);
        integerBinaries.put("0 > 0", false);  integerBinaries.put("0 < 0", false);
        integerBinaries.put("0 >= 0", true);  integerBinaries.put("0 <= 0", true);
        integerBinaries.put("0 == 0", true);  integerBinaries.put("0 != 0", false);
        integerBinaries.put("10 + 15", 25);   integerBinaries.put("20 - 10", 10);
        integerBinaries.put("10 * 15", 150);  integerBinaries.put("90 / 10", 9);
        integerBinaries.put("30 % 8", 6);     integerBinaries.put("5 ^ 5", 3125);
        integerBinaries.put("5 > 4", true);   integerBinaries.put("4 > 5", false);
        integerBinaries.put("5 < 4", false);  integerBinaries.put("4 < 5", true);
        integerBinaries.put("4 >= 5", false); integerBinaries.put("5 >= 4", true);
        integerBinaries.put("5 >= 5", true);  integerBinaries.put("4 <= 5", true);
        integerBinaries.put("5 <= 4", false); integerBinaries.put("5 <= 5", true);
        integerBinaries.put("4 == 5", false); integerBinaries.put("5 == 5", true);
        integerBinaries.put("4 != 5", true);  integerBinaries.put("5 != 5", false);

        return integerBinaries;
    }

    static HashMap<String, Object> binaryDoubleCollection()
    {
        HashMap<String, Object> doubleBinaries = new HashMap<>();
        doubleBinaries.put("0.0 + 0.0", 0.0);      doubleBinaries.put("0.0 - 0.0", 0.0);
        doubleBinaries.put("0.0 * 0.0", 0.0);      doubleBinaries.put("0.0 / 0.0", 0.0);
        doubleBinaries.put("0.0 % 0.0", 0.0);      doubleBinaries.put("0.0 ^ 0.0", 1.0);
        doubleBinaries.put("0.0 > 0.0", false);    doubleBinaries.put("0.0 < 0.0", false);
        doubleBinaries.put("0.0 >= 0.0", true);    doubleBinaries.put("0.0 <= 0.0", true);
        doubleBinaries.put("0.0 == 0.0", true);    doubleBinaries.put("0.0 != 0.0", false);
        doubleBinaries.put("10.5 + 15.5", 26.0);     doubleBinaries.put("20.5 - 10.5", 10.0);
        doubleBinaries.put("10.5 * 15.5", 162.75); doubleBinaries.put("91.8 / 10.2", 9.0);
        doubleBinaries.put("30.2 % 8.6", 4.4);     doubleBinaries.put("5.2 ^ 5.2", 5287.098322295948);
        doubleBinaries.put("5.0 > 4.9", true);     doubleBinaries.put("4.9 > 5.5", false);
        doubleBinaries.put("5.0 < 4.9", false);    doubleBinaries.put("4.9 < 5.5", true);
        doubleBinaries.put("4.5 >= 5.5", false);   doubleBinaries.put("5.5 >= 4.9", true);
        doubleBinaries.put("5.5 >= 5.5", true);    doubleBinaries.put("4.5 <= 5.5", true);
        doubleBinaries.put("5.5 <= 4.0", false);   doubleBinaries.put("5.0 <= 5.0", true);
        doubleBinaries.put("4.0 == 5.5", false);   doubleBinaries.put("5.5 == 5.5", true);
        doubleBinaries.put("4.0 != 5.5", true);    doubleBinaries.put("5.5 != 5.5", false);

        return doubleBinaries;
    }

    static HashMap<String, Object> binaryBooleanCollection()
    {
        HashMap<String, Object> booleanBinaries = new HashMap<>();
        booleanBinaries.put("true AND true", true);   booleanBinaries.put("false AND false", false);
        booleanBinaries.put("true AND false", false); booleanBinaries.put("true OR true", true);
        booleanBinaries.put("false OR false", false); booleanBinaries.put("true OR false", true);
        booleanBinaries.put("true == true", true);    booleanBinaries.put("false == false", true);
        booleanBinaries.put("true == false", false);  booleanBinaries.put("true != true", false);
        booleanBinaries.put("false != false", false); booleanBinaries.put("true != false", true);

        return booleanBinaries;
    }

    static HashMap<String, Object> binaryStringCollection()
    {
        HashMap<String, Object> stringBinaries = new HashMap<>();
        stringBinaries.put("\"string\" + \"string\"", "stringstring");
        stringBinaries.put("\"separated \" + \"string\"", "separated string");
        stringBinaries.put("\"string\" == \"string\"", true);
        stringBinaries.put("\"string\" != \"string\"", false);

        return stringBinaries;
    }

    static HashMap<String, Object> identifierCollection()
    {
        HashMap<String, Object> identifiers = new HashMap<>();
        identifiers.put("a", "a");
        identifiers.put("variable", "variable");
        identifiers.put("my_var", "my_var");
        identifiers.put("camelCaseVariable", "camelCaseVariable");
        identifiers.put("CAPITALVARIABLE", "CAPITALVARIABLE");

        return identifiers;
    }

    static HashMap<String, Object> assignmentCollection()
    {
        HashMap<String, Object> assignments = new HashMap<>();
        assignments.put("a=0", 0);       assignments.put("a=0.0", 0.0);
        assignments.put("a=10", 10);     assignments.put("a=10.0", 10.0);
        assignments.put("a=10*5", 50);   assignments.put("a=10.0*5.0", 50.0);
        assignments.put("a=true", true); assignments.put("a=false", false);
        assignments.put("a=\"string\"", "string");
        assignments.put("a=\"string\"+\"string\"", "stringstring");

        return assignments;
    }

    static HashMap<String[], Object> assignmentOperatorsCollection()
    {
        HashMap<String[], Object> assignmentOperators = new HashMap<>();
        assignmentOperators.put(new String[]{"a=20", "a+=10"}, 30);
        assignmentOperators.put(new String[]{"a=20", "a-=10"}, 10);
        assignmentOperators.put(new String[]{"a=20", "a*=10"}, 200);
        assignmentOperators.put(new String[]{"a=20", "a/=10"}, 2);
        assignmentOperators.put(new String[]{"a=20", "a%=12"}, 8);
        assignmentOperators.put(new String[]{"a=10", "a^=2"}, 100);
        assignmentOperators.put(new String[]{"a=20.5", "a+=10.5"}, 31.0);
        assignmentOperators.put(new String[]{"a=20.5", "a-=10.5"}, 10.0);
        assignmentOperators.put(new String[]{"a=20.0", "a*=10.0"}, 200.0);
        assignmentOperators.put(new String[]{"a=20.0", "a/=10.0"}, 2.0);
        assignmentOperators.put(new String[]{"a=20.0", "a%=12.0"}, 8.0);
        assignmentOperators.put(new String[]{"a=10.0", "a^=2.0"}, 100.0);
        assignmentOperators.put(new String[]{"a=\"string\"", "a+=\" string\""}, "string string");

        return assignmentOperators;
    }

    static HashMap<String[], Object> reassignmentCollection()
    {
        HashMap<String[], Object> reassignmentCollection = new HashMap<>();
        reassignmentCollection.put(new String[]{"a=20", "a=10"}, 10);
        reassignmentCollection.put(new String[]{"a=20", "a=10.0"}, 10.0);
        reassignmentCollection.put(new String[]{"a=20", "a=false"}, false);
        reassignmentCollection.put(new String[]{"a=20", "a=\"string\""}, "string");
        reassignmentCollection.put(new String[]{"a=20.0", "a=10"}, 10);
        reassignmentCollection.put(new String[]{"a=20.0", "a=10.0"}, 10.0);
        reassignmentCollection.put(new String[]{"a=20.0", "a=false"}, false);
        reassignmentCollection.put(new String[]{"a=20.0", "a=\"string\""}, "string");
        reassignmentCollection.put(new String[]{"a=true", "a=10"}, 10);
        reassignmentCollection.put(new String[]{"a=true", "a=10.0"}, 10.0);
        reassignmentCollection.put(new String[]{"a=true", "a=false"}, false);
        reassignmentCollection.put(new String[]{"a=true", "a=\"string\""}, "string");
        reassignmentCollection.put(new String[]{"a=\"string\"", "a=10"}, 10);
        reassignmentCollection.put(new String[]{"a=\"string\"", "a=10.0"}, 10.0);
        reassignmentCollection.put(new String[]{"a=\"string\"", "a=false"}, false);
        reassignmentCollection.put(new String[]{"a=\"string\"", "a=\"string\""}, "string");

        return reassignmentCollection;
    }

    static HashMap<String, TokenType> tokenTypeCollection()
    {
        HashMap<String, TokenType> tokenTypes = new HashMap<>();
        tokenTypes.put("10", TokenType.INTEGER_TOKEN);
        tokenTypes.put("10.0", TokenType.DOUBLE_TOKEN);
        tokenTypes.put("\"string\"", TokenType.STRING_TOKEN);
        tokenTypes.put("var", TokenType.IDENTIFIER_TOKEN);
        tokenTypes.put(Syntax.TRUE.getSyntax(), TokenType.TRUE_KEYWORD_TOKEN);
        tokenTypes.put(Syntax.FALSE.getSyntax(), TokenType.FALSE_KEYWORD_TOKEN);
        tokenTypes.put(Syntax.AND.getSyntax(), TokenType.AND_TOKEN);
        tokenTypes.put(Syntax.OR.getSyntax(), TokenType.OR_TOKEN);
        tokenTypes.put(Syntax.OPEN_BRACE.getSyntax(), TokenType.OPEN_BRACE_TOKEN);
        tokenTypes.put(Syntax.CLOSE_BRACE.getSyntax(), TokenType.CLOSE_BRACE_TOKEN);
        tokenTypes.put(Syntax.OPEN_PARENTHESIS.getSyntax(), TokenType.OPEN_PARENTHESIS_TOKEN);
        tokenTypes.put(Syntax.CLOSE_PARENTHESIS.getSyntax(), TokenType.CLOSE_PARENTHESIS_TOKEN);
        tokenTypes.put(Syntax.PLUS.getSyntax(), TokenType.PLUS_TOKEN);
        tokenTypes.put(Syntax.MINUS.getSyntax(), TokenType.MINUS_TOKEN);
        tokenTypes.put(Syntax.STAR.getSyntax(), TokenType.STAR_TOKEN);
        tokenTypes.put(Syntax.SLASH.getSyntax(), TokenType.SLASH_TOKEN);
        tokenTypes.put(Syntax.PERCENT.getSyntax(), TokenType.PERCENT_TOKEN);
        tokenTypes.put(Syntax.CARET.getSyntax(), TokenType.CARET_TOKEN);
        tokenTypes.put(Syntax.GREATER.getSyntax(), TokenType.GREATER_TOKEN);
        tokenTypes.put(Syntax.LESS.getSyntax(), TokenType.LESS_TOKEN);
        tokenTypes.put(Syntax.GREATER_EQUALS.getSyntax(), TokenType.GREATER_EQUALS_TOKEN);
        tokenTypes.put(Syntax.LESS_EQUALS.getSyntax(), TokenType.LESS_EQUALS_TOKEN);
        tokenTypes.put(Syntax.NOT.getSyntax(), TokenType.NOT_TOKEN);
        tokenTypes.put(Syntax.EQUALS.getSyntax(), TokenType.EQUALS_TOKEN);
        tokenTypes.put(Syntax.EQUALS_EQUALS.getSyntax(), TokenType.EQUALS_EQUALS_TOKEN);
        tokenTypes.put(Syntax.NOT_EQUALS.getSyntax(), TokenType.NOT_EQUALS_TOKEN);

        return tokenTypes;
    }

    static HashMap<String, String> lexicalErrorCollection()
    {
        HashMap<String, String> lexicalErrors = new HashMap<>();
        String input, output;
        LexicalError error;

        input = "2147483648";
        error = LexicalError.invalidInt(input, 0, 10);
        output = getErrorOutput(input, error);
        lexicalErrors.put(input, output);

        input = "@";
        error = LexicalError.badCharacter(input, 0, 1);
        output = getErrorOutput(input, error);
        lexicalErrors.put(input, output);

        input = "\"string";
        error = LexicalError.incompleteString(input, 0, 7);
        output = getErrorOutput(input, error);
        lexicalErrors.put(input, output);

        return lexicalErrors;
    }

    static HashMap<String, String> syntaxErrorCollection()
    {
        HashMap<String, String> syntaxErrors = new HashMap<>();
        String input, output;
        SourceSpan span;
        SyntaxError error;

        input = "(";
        span = new SourceSpan(1, 1);
        error = SyntaxError.unexpectedTokenMatch(span, TokenType.EOF_TOKEN, TokenType.CLOSE_PARENTHESIS_TOKEN);
        output = getErrorOutput(input, error);
        syntaxErrors.put(input, output);

        input = "{";
        span = new SourceSpan(1, 1);
        error = SyntaxError.unexpectedTokenMatch(span, TokenType.EOF_TOKEN, TokenType.CLOSE_BRACE_TOKEN);
        output = getErrorOutput(input, error);
        syntaxErrors.put(input, output);

        return syntaxErrors;
    }

    static HashMap<String, String> semanticErrorCollection()
    {
        HashMap<String, String> semanticErrors = new HashMap<>();
        String input, output;
        SourceSpan span;
        SemanticError error;

        input = "+true";
        span = new SourceSpan(0, 1);
        error = SemanticError.undefinedUnaryOperator(span, "+", ObjectType.BOOLEAN_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "!5";
        span = new SourceSpan(0, 1);
        error = SemanticError.undefinedUnaryOperator(span, "!", ObjectType.INTEGER_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "!5.0";
        span = new SourceSpan(0, 1);
        error = SemanticError.undefinedUnaryOperator(span, "!", ObjectType.DOUBLE_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "true + false";
        span = new SourceSpan(5, 1);
        error = SemanticError.undefinedBinaryOperator(span, "+", ObjectType.BOOLEAN_OBJECT, ObjectType.BOOLEAN_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "5 * 5.0";
        span = new SourceSpan(2, 1);
        error = SemanticError.undefinedBinaryOperator(span, "*", ObjectType.INTEGER_OBJECT, ObjectType.DOUBLE_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "\"string\" * true";
        span = new SourceSpan(9, 1);
        error = SemanticError.undefinedBinaryOperator(span, "*", ObjectType.STRING_OBJECT, ObjectType.BOOLEAN_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "a += 5";
        span = new SourceSpan(2, 2);
        error = SemanticError.undefinedAssignmentOperator(span, "+=", ObjectType.NULL_OBJECT, ObjectType.INTEGER_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "a *= true";
        span = new SourceSpan(2, 2);
        error = SemanticError.undefinedAssignmentOperator(span, "*=", ObjectType.NULL_OBJECT, ObjectType.BOOLEAN_OBJECT);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        input = "variable";
        span = new SourceSpan(0, 8);
        error = SemanticError.undefinedIdentifier(span, input);
        output = getErrorOutput(input, error);
        semanticErrors.put(input, output);

        return semanticErrors;
    }

    private static String getErrorOutput(String input, Error error)
    {
        ErrorHandler errorHandler = TestHub.createErrorHandler(input);
        errorHandler.addError(error);
        return errorHandler.getErrors();
    }
}



