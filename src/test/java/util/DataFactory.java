package util;

import compilation.analysis.lexical.Token;
import errors.Error;
import errors.LexicalError;
import errors.SemanticError;
import errors.SyntaxError;
import source.SourceSpan;
import types.TokenType;

import java.util.HashMap;

import static compilation.analysis.lexical.Syntax.*;
import static types.ObjectType.*;
import static types.TokenType.*;

final class DataFactory
{
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

    static HashMap<String, Object> assignmentOperatorsCollection()
    {
        HashMap<String, Object> assignmentOperators = new HashMap<>();
        assignmentOperators.put("a=20\na+=10\na", 30);
        assignmentOperators.put("a=20\na-=10\na", 10);
        assignmentOperators.put("a=20\na*=10\na", 200);
        assignmentOperators.put("a=20\na/=10\na", 2);
        assignmentOperators.put("a=20\na%=12\na", 8);
        assignmentOperators.put("a=10\na^=2\na", 100);
        assignmentOperators.put("a=20.5\na+=10.5\na", 31.0);
        assignmentOperators.put("a=20.5\na-=10.5\na", 10.0);
        assignmentOperators.put("a=20.0\na*=10.0\na", 200.0);
        assignmentOperators.put("a=20.0\na/=10.0\na", 2.0);
        assignmentOperators.put("a=20.0\na%=12.0\na", 8.0);
        assignmentOperators.put("a=10.0\na^=2.0\na", 100.0);
        assignmentOperators.put("a=\"string\"\na+=\" string\"\na", "string string");

        return assignmentOperators;
    }

    static HashMap<String, Object> reassignmentCollection()
    {
        HashMap<String, Object> reassignmentCollection = new HashMap<>();
        reassignmentCollection.put("a=20\na=10\na", 10);
        reassignmentCollection.put("a=20\na=10.0\na", 10.0);
        reassignmentCollection.put("a=20\na=false\na", false);
        reassignmentCollection.put("a=20\na=\"string\"\na", "string");
        reassignmentCollection.put("a=20.0\na=10\na", 10);
        reassignmentCollection.put("a=20.0\na=10.0\na", 10.0);
        reassignmentCollection.put("a=20.0\na=false\na", false);
        reassignmentCollection.put("a=20.0\na=\"string\"\na", "string");
        reassignmentCollection.put("a=true\na=10\na", 10);
        reassignmentCollection.put("a=true\na=10.0\na", 10.0);
        reassignmentCollection.put("a=true\na=false\na", false);
        reassignmentCollection.put("a=true\na=\"string\"\na", "string");
        reassignmentCollection.put("a=\"string\"\na=10\na", 10);
        reassignmentCollection.put("a=\"string\"\na=10.0\na", 10.0);
        reassignmentCollection.put("a=\"string\"\na=false\na", false);
        reassignmentCollection.put("a=\"string\"\na=\"string\"\na", "string");

        return reassignmentCollection;
    }

    static HashMap<String, TokenType> tokenTypeCollection()
    {
        HashMap<String, TokenType> tokenTypes = new HashMap<>();
        tokenTypes.put("10", INTEGER_TOKEN);
        tokenTypes.put("10.0", DOUBLE_TOKEN);
        tokenTypes.put("\"string\"", STRING_TOKEN);
        tokenTypes.put("var", IDENTIFIER_TOKEN);
        tokenTypes.put(TRUE_SYNTAX, BOOLEAN_TOKEN);
        tokenTypes.put(FALSE_SYNTAX, BOOLEAN_TOKEN);
        tokenTypes.put(AND_SYNTAX, AND_TOKEN);
        tokenTypes.put(OR_SYNTAX, OR_TOKEN);
        tokenTypes.put(OPEN_BRACE_SYNTAX, OPEN_BRACE_TOKEN);
        tokenTypes.put(CLOSE_BRACE_SYNTAX, CLOSE_BRACE_TOKEN);
        tokenTypes.put(OPEN_PARENTHESIS_SYNTAX, OPEN_PARENTHESIS_TOKEN);
        tokenTypes.put(CLOSE_PARENTHESIS_SYNTAX, CLOSE_PARENTHESIS_TOKEN);
        tokenTypes.put(PLUS_SYNTAX, PLUS_TOKEN);
        tokenTypes.put(MINUS_SYNTAX, MINUS_TOKEN);
        tokenTypes.put(STAR_SYNTAX, STAR_TOKEN);
        tokenTypes.put(SLASH_SYNTAX, SLASH_TOKEN);
        tokenTypes.put(PERCENT_SYNTAX, PERCENT_TOKEN);
        tokenTypes.put(CARET_SYNTAX, CARET_TOKEN);
        tokenTypes.put(GREATER_SYNTAX, GREATER_TOKEN);
        tokenTypes.put(LESS_SYNTAX, LESS_TOKEN);
        tokenTypes.put(GREATER_EQUALS_SYNTAX, GREATER_EQUALS_TOKEN);
        tokenTypes.put(LESS_EQUALS_SYNTAX, LESS_EQUALS_TOKEN);
        tokenTypes.put(NOT_SYNTAX, NOT_TOKEN);
        tokenTypes.put(EQUALS_SYNTAX, EQUALS_TOKEN);
        tokenTypes.put(EQUALS_EQUALS_SYNTAX, EQUALS_EQUALS_TOKEN);
        tokenTypes.put(NOT_EQUALS_SYNTAX, NOT_EQUALS_TOKEN);

        return tokenTypes;
    }

    static HashMap<String, Error> lexicalErrorCollection()
    {
        HashMap<String, Error> lexicalErrors = new HashMap<>();
        String input;
        LexicalError error;

        input = "2147483648";
        error = LexicalError.invalidInt(input, 0, 10);
        lexicalErrors.put(input, error);

        input = "@";
        error = LexicalError.badCharacter(input, 0, 1);
        lexicalErrors.put(input, error);

        input = "\"string";
        error = LexicalError.incompleteString(input, 0, 7);
        lexicalErrors.put(input, error);

        return lexicalErrors;
    }

    static HashMap<String, Error> syntaxErrorCollection()
    {
        HashMap<String, Error> syntaxErrors = new HashMap<>();
        String input;
        Token token;
        SyntaxError error;

        input = "(";
        token = new Token(EOF_TOKEN, EOF_SYNTAX, 1);
        error = SyntaxError.invalidToken(token.getSpan(), token);
        syntaxErrors.put(input, error);

        input = "{";
        token = new Token(OPEN_BRACE_TOKEN, OPEN_BRACE_SYNTAX, 0);
        error = SyntaxError.invalidToken(token.getSpan(), token);
        syntaxErrors.put(input, error);

        return syntaxErrors;
    }

    static HashMap<String, Error> semanticErrorCollection()
    {
        HashMap<String, Error> semanticErrors = new HashMap<>();
        String input;
        SourceSpan span;
        SemanticError error;

        input = "+true";
        span = new SourceSpan(0, 1);
        error = SemanticError.undefinedUnaryOperator(span, "+", BOOLEAN_OBJECT);
        semanticErrors.put(input, error);

        input = "!5";
        span = new SourceSpan(0, 1);
        error = SemanticError.undefinedUnaryOperator(span, "!", INTEGER_OBJECT);
        semanticErrors.put(input, error);

        input = "!5.0";
        span = new SourceSpan(0, 1);
        error = SemanticError.undefinedUnaryOperator(span, "!", DOUBLE_OBJECT);
        semanticErrors.put(input, error);

        input = "true + false";
        span = new SourceSpan(5, 1);
        error = SemanticError.undefinedBinaryOperator(span, "+", BOOLEAN_OBJECT, BOOLEAN_OBJECT);
        semanticErrors.put(input, error);

        input = "5 * 5.0";
        span = new SourceSpan(2, 1);
        error = SemanticError.undefinedBinaryOperator(span, "*", INTEGER_OBJECT, DOUBLE_OBJECT);
        semanticErrors.put(input, error);

        input = "\"string\" * true";
        span = new SourceSpan(9, 1);
        error = SemanticError.undefinedBinaryOperator(span, "*", STRING_OBJECT, BOOLEAN_OBJECT);
        semanticErrors.put(input, error);

        input = "a += 5";
        span = new SourceSpan(2, 2);
        error = SemanticError.undefinedAssignmentOperator(span, "+=", NULL_OBJECT, INTEGER_OBJECT);
        semanticErrors.put(input, error);

        input = "a *= true";
        span = new SourceSpan(2, 2);
        error = SemanticError.undefinedAssignmentOperator(span, "*=", NULL_OBJECT, BOOLEAN_OBJECT);
        semanticErrors.put(input, error);

        input = "variable";
        span = new SourceSpan(0, 8);
        error = SemanticError.undefinedIdentifier(span, input);
        semanticErrors.put(input, error);

        return semanticErrors;
    }
}
