/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.parsing;

import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

import static mavlc.parsing.Token.TokenType.*;
import static mavlc.syntax.expression.Compare.Comparison.*;

/* TODO enter group information
 *
 * EiCB group number: 20
 * Names and matriculation numbers of all group members:
 * Devran Akdag
 * Jehad Sefi
 * Minh Huy Tran mt45qyry
 */

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {
	
	private final Deque<Token> tokens;
	private Token currentToken;
	
	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}
	
	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}
	
	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}
	
	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}
	
	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();
		
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);
		
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);
		
		return new Function(location, name, typeSpecifier, parameters, body);
	}
	
	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		return new FormalParameter(location, name, typeSpecifier);
	}
	
	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;
		accept(RECORD);
		String name = accept(ID);
		accept(LBRACE);
		List<RecordElementDeclaration> elements = new LinkedList<RecordElementDeclaration>();
		RecordElementDeclaration element;
		do { // recordElemDecl+
			element = parseRecordElementDeclaration();
			elements.add(element);
		} while (currentToken.type == VAR || currentToken.type == VAL); // Von starters[recordElemDecl]
		accept(RBRACE);
		RecordTypeDeclaration result = new RecordTypeDeclaration(location, name, elements);
		return result;
	}
	
	private RecordElementDeclaration parseRecordElementDeclaration() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;
		boolean isVariable = false;
		switch(currentToken.type) {
		case VAR:
			isVariable = true;
		case VAL:
			acceptIt();
			break;
		default:
			throw new SyntaxError(currentToken, VAR, VAL);
		}
		TypeSpecifier type = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);
		RecordElementDeclaration result = new RecordElementDeclaration(location, isVariable, type, name);
		return result;
	}
	
	private IteratorDeclaration parseIteratorDeclaration() {
		// TODO implement (task 1.6)
		SourceLocation location = currentToken.sourceLocation;
		boolean isVariable = false;
		switch(currentToken.type) {
		case VAR:
			isVariable = true;
		case VAL:
			acceptIt();
			break;
		default:
			throw new SyntaxError(currentToken, VAR, VAL);
		}
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		IteratorDeclaration result = new IteratorDeclaration(location, name, typeSpecifier, isVariable);
		return result;
	}
	
	private TypeSpecifier parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}
		
		accept(LANGLE);
		TypeSpecifier subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);
		
		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);
		
		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);
		
		return new MatrixTypeSpecifier(location, subtype, x, y);
	}
	
	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}
	
	private ValueDefinition parseValueDef() {
		// TODO implement (task 1.1)
		SourceLocation location = currentToken.sourceLocation;
		ValueDefinition result;
		accept(VAL);
		TypeSpecifier type = parseTypeSpecifier();
		String identifier = accept(ID);
		accept(ASSIGN);
		Expression expr = parseExpr();
		accept(SEMICOLON);
		result = new ValueDefinition(location, type, identifier, expr);
		return result;
	}
	
	private VariableDeclaration parseVarDecl() {
		// TODO implement (task 1.1)
		SourceLocation location = currentToken.sourceLocation;
		VariableDeclaration result;
		accept(VAR);
		TypeSpecifier type = parseTypeSpecifier();
		String identifier = accept(ID);
		accept(SEMICOLON);
		result = new VariableDeclaration(location, type, identifier);
		return result;
	}
	
	private ReturnStatement parseReturn() {
		// TODO implement (task 1.5)
		SourceLocation location = currentToken.sourceLocation;
		accept(RETURN);
		Expression expr = parseExpr();
		accept(SEMICOLON);
		return new ReturnStatement(location, expr);
	}
	
	private Statement parseAssignOrCall() {
		SourceLocation location = currentToken.sourceLocation;
		
		String name = accept(ID);
		
		Statement s;
		if(currentToken.type != LPAREN)
			s = parseAssign(name, location);
		else
			s = new CallStatement(location, parseCall(name, location));
		accept(SEMICOLON);
		
		return s;
	}
	
	private VariableAssignment parseAssign(String name, SourceLocation location) {
		// TODO implement (task 1.1)
		VariableAssignment result;
		LeftHandIdentifier id = new LeftHandIdentifier(location, name);
		if (currentToken.type != ASSIGN) {
			switch(currentToken.type) {
			case LBRACKET: // Matrix oder Vector
				acceptIt();
				Expression expr1 = parseExpr();
				accept(RBRACKET);
				if (currentToken.type == LBRACKET) { // Falls Matrix:
					acceptIt();
					Expression expr2 = parseExpr();
					accept(RBRACKET);
					id = new MatrixLhsIdentifier(location, name, expr1, expr2);
					break; // Ende Switch
				}
				id = new VectorLhsIdentifier(location, name, expr1); // Falls Vector
				break;
			case AT:  // Recordselement
				acceptIt();
				String element = accept(ID);
				id = new RecordLhsIdentifier(location, name, element);
				break;
			default: // Syntaxfehler
				throw new SyntaxError(currentToken, LBRACKET, AT, ASSIGN);
			}
		}
		accept(ASSIGN);
		Expression value = parseExpr();
		result = new VariableAssignment(location, id, value);
		return result;
	}
	
	private CallExpression parseCall(String name, SourceLocation location) {
		// TODO implement (task 1.5)
		CallExpression result;
		List<Expression> actualParameters = new LinkedList<Expression>();
		accept(LPAREN);
		if (currentToken.type != RPAREN) {
			Expression parameter = parseExpr();
			actualParameters.add(parameter);
			while (currentToken.type == COMMA) {
				acceptIt();
				parameter = parseExpr();
				actualParameters.add(parameter);
			}
		}
		accept(RPAREN);
		result = new CallExpression(location, name, actualParameters);
		return result;
	}
	
	private ForLoop parseFor() {
		// TODO implement (task 1.6)
		SourceLocation location = currentToken.sourceLocation;
		accept(FOR);
		accept(LPAREN);
		String init = accept(ID);
		accept(ASSIGN);
		Expression initExpr = parseExpr();
		accept(SEMICOLON);
		Expression loopCon = parseExpr();
		accept(SEMICOLON);
		String incrVar = accept(ID);
		accept(ASSIGN);
		Expression incrExpr = parseExpr();
		accept(RPAREN);
		Statement statement = parseStatement();
		ForLoop result = new ForLoop(location, init, initExpr, loopCon, incrVar, incrExpr, statement);
		return result;
	}
	
	private ForEachLoop parseForEach() {
		// TODO implement (task 1.6)
		SourceLocation location = currentToken.sourceLocation;
		accept(FOREACH);
		accept(LPAREN);
		IteratorDeclaration iteratorDecl = parseIteratorDeclaration();
		accept(COLON);
		Expression expr = parseExpr();
		accept(RPAREN);
		Statement statement = parseStatement();
		ForEachLoop result = new ForEachLoop(location, iteratorDecl, expr, statement);
		return result;
	}
	
	private IfStatement parseIf() {
		// TODO implement (task 1.4)
		SourceLocation location = currentToken.sourceLocation;
		accept(IF);
		accept(LPAREN);
		Expression expr = parseExpr();
		accept(RPAREN);
		Statement statement1 = parseStatement();
		IfStatement result = new IfStatement(location, expr, statement1); // Für nur If-Then Statement
		if (currentToken.type == ELSE) { // Falls gibt noch Else-Teil
			acceptIt();
			Statement statement2 = parseStatement();
			result = new IfStatement(location, expr, statement1, statement2);
		}
		return result;
	}
	
	private SwitchStatement parseSwitch() {
		SourceLocation location = currentToken.sourceLocation;
		accept(SWITCH);
		accept(LPAREN);
		Expression condition = parseExpr();
		accept(RPAREN);
		accept(LBRACE);
		
		List<Case> cases = new ArrayList<>();
		List<Default> defaults = new ArrayList<>();
		while(currentToken.type != RBRACE) {
			if(currentToken.type == CASE)
				cases.add(parseCase());
			else if(currentToken.type == DEFAULT)
				defaults.add(parseDefault());
			else
				throw new SyntaxError(currentToken, CASE, DEFAULT);
		}
		
		accept(RBRACE);
		return new SwitchStatement(location, condition, cases, defaults);
	}
	
	private Case parseCase() {
		SourceLocation location = currentToken.sourceLocation;
		
		accept(CASE);
		Expression caseCond = parseExpr();
		accept(COLON);
		Statement stmt = parseStatement();
		return new Case(location, caseCond, stmt);
	}
	
	private Default parseDefault() {
		SourceLocation location = currentToken.sourceLocation;
		
		accept(DEFAULT);
		accept(COLON);
		return new Default(location, parseStatement());
	}
	
	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Statement> statements = new ArrayList<>();
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			statements.add(parseStatement());
		accept(RBRACE);
		
		return new CompoundStatement(location, statements);
	}
	
	private Expression parseExpr() {
		return parseSelect();
	}
	
	private Expression parseSelect() {
		// TODO implement (task 1.3)
		SourceLocation location = currentToken.sourceLocation;
		
		Expression expr1 = parseOr();
		while (currentToken.type == QMARK) {
			acceptIt();
			Expression expr2 = parseOr();
			accept(COLON);
			Expression expr3 = parseOr();
			expr1 = new SelectExpression(location, expr1, expr2, expr3);
		}
		return expr1;
	}
	
	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}
	
	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}
	
	private Expression parseNot() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == NOT) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			return new Not(location, parseCompare());
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		return parseCompare();
	}
	
	private Expression parseCompare() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression expr1 = parseAddSub();
		while (currentToken.type == LANGLE || currentToken.type == RANGLE || currentToken.type == CMPLE ||
				currentToken.type == CMPGE || currentToken.type == CMPEQ || currentToken.type == CMPNE) {
			Expression expr2;
			switch(currentToken.type) {
			case LANGLE: // <
				acceptIt();
				expr2 = parseAddSub();
				expr1 = new Compare(location, expr1, expr2, LESS);
				break;
			case RANGLE: // >
				acceptIt();
				expr2 = parseAddSub();
				expr1 = new Compare(location, expr1, expr2, GREATER);
				break;
			case CMPLE: // <=
				acceptIt();
				expr2 = parseAddSub();
				expr1 = new Compare(location, expr1, expr2, LESS_EQUAL);
				break;
			case CMPGE: // >=
				acceptIt();
				expr2 = parseAddSub();
				expr1 = new Compare(location, expr1, expr2, GREATER_EQUAL);
				break;
			case CMPEQ: // ==
				acceptIt();
				expr2 = parseAddSub();
				expr1 = new Compare(location, expr1, expr2, EQUAL);
				break;
			case CMPNE: // !=
				acceptIt();
				expr2 = parseAddSub();
				expr1 = new Compare(location, expr1, expr2, NOT_EQUAL);
				break;
			default:
				throw new SyntaxError(currentToken, LANGLE, RANGLE, CMPLE, CMPGE, CMPEQ, CMPNE);
			}
		}
		return expr1;
	}
	
	private Expression parseAddSub() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression expr1 = parseMulDiv();
		while (currentToken.type == ADD || currentToken.type == SUB) {
			boolean isAdd = (currentToken.type == ADD)? true : false;
			acceptIt();
			Expression expr2 = parseMulDiv();
			if (isAdd) {
				expr1 = new Addition(location ,expr1, expr2);
			}
			else {
				expr1 = new Subtraction(location, expr1, expr2);
			}
		}
		return expr1;
	}
	
	private Expression parseMulDiv() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression expr1 = parseUnaryMinus();
		while (currentToken.type == MULT || currentToken.type == DIV) {
			boolean isMult = (currentToken.type == MULT)? true : false;
			acceptIt();
			Expression expr2 = parseUnaryMinus();
			if (isMult) {
				expr1 = new Multiplication(location ,expr1, expr2);
			}
			else {
				expr1 = new Division(location, expr1, expr2);
			}
		}
		return expr1;
	}
	
	private Expression parseUnaryMinus() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == SUB) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			return new UnaryMinus(location, parseExponentiation());
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		return parseExponentiation();
	}
	
	/**
	 * Rekursive Methode für rechtsassoziative Exponentiation
	 * @return die ganze Exponentiation-Expression mit allen Operanden
	 */
	private Expression parseExponentiation() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression expr1 = parseDotProd();
		if (currentToken.type == EXP) {
			acceptIt();
			return new Exponentiation(location, expr1, parseExponentiation()); // Falls es noch Operanden gibt
		}
		return expr1; // Falls es nur 1 Operand gibt
	}
	
	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}
	
	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}
	
	private Expression parseTranspose() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == TRANSPOSE) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			 return new MatrixTranspose(location, parseDim());
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		return parseDim();
	}
	
	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}
	
	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseElementSelect();
		
		if(currentToken.type == LBRACE) {
			acceptIt();
			Expression xStartIndex = parseExpr();
			accept(COLON);
			Expression xBaseIndex = parseExpr();
			accept(COLON);
			Expression xEndIndex = parseExpr();
			accept(RBRACE);
			if(currentToken.type != LBRACE)
				return new SubVector(location, x, xBaseIndex, xStartIndex, xEndIndex);
			
			accept(LBRACE);
			Expression yStartIndex = parseExpr();
			accept(COLON);
			Expression yBaseIndex = parseExpr();
			accept(COLON);
			Expression yEndIndex = parseExpr();
			accept(RBRACE);
			return new SubMatrix(location, x, xBaseIndex, xStartIndex, xEndIndex, yBaseIndex, yStartIndex, yEndIndex);
		}
		
		return x;
	}
	
	private Expression parseElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseRecordElementSelect();
		
		while(currentToken.type == LBRACKET) {
			acceptIt();
			Expression idx = parseExpr();
			accept(RBRACKET);
			x = new ElementSelect(location, x, idx);
		}
		
		return x;
	}
	
	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAtom();
		
		if(currentToken.type == AT) {
			accept(AT);
			String elementName = accept(ID);
			x = new RecordElementSelect(location, x, elementName);
		}
		
		return x;
	}
	
	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;
		
		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default: /* check other cases below */
		}
		
		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
				
			} else {
				return parseCall(name, location);
			}
		}
		
		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}
		
		if(currentToken.type == AT) {
		// TODO implement (task 1.7)
			acceptIt();
			String name = accept(ID);
			return new RecordInit(location, name, parseInitializerList());
		}
		
		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}
		
		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}
	
	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();
		
		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);
		
		return elements;
	}
	
	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}
	
	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}
	
	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}
