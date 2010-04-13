/* This file is part of the FaCT++ DL reasoner
Copyright (C) 2010 by Dmitry Tsarkov

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef TDLEXPRESSION_H
#define TDLEXPRESSION_H

#include <vector>
#include <string>

#include "eFaCTPlusPlus.h"

// forward declaration for all expression classes: necessary for the visitor pattern
class TDLExpression;

class  TDLConceptExpression;
class   TDLConceptTop;
class   TDLConceptBottom;
class   TDLConceptName;
class   TDLConceptNot;
class   TDLConceptAnd;
class   TDLConceptOr;
class   TDLConceptOneOf;
class   TDLConceptObjectRoleExpression;
class    TDLConceptObjectSelf;
class    TDLConceptObjectValue;
class    TDLConceptObjectRCExpression;
class     TDLConceptObjectExists;
class     TDLConceptObjectForall;
class     TDLConceptObjectCardinalityExpression;
class      TDLConceptObjectMinCardinality;
class      TDLConceptObjectMaxCardinality;
class      TDLConceptObjectExactCardinality;
class   TDLConceptDataRoleExpression;
class    TDLConceptDataValue;
class    TDLConceptDataRVExpression;
class     TDLConceptDataExists;
class     TDLConceptDataForall;
class     TDLConceptDataCardinalityExpression;
class      TDLConceptDataMinCardinality;
class      TDLConceptDataMaxCardinality;
class      TDLConceptDataExactCardinality;

class  TDLIndividualExpression;
class   TDLIndividualName;

class  TDLRoleExpression;
class   TDLObjectRoleComplexExpression;
class    TDLObjectRoleExpression;
class     TDLObjectRoleTop;
class     TDLObjectRoleBottom;
class     TDLObjectRoleName;
class     TDLObjectRoleInverse;
class    TDLObjectRoleChain;
class    TDLObjectRoleProjectionFrom;
class    TDLObjectRoleProjectionInto;

class   TDLDataRoleExpression;
class    TDLDataRoleTop;
class    TDLDataRoleBottom;
class    TDLDataRoleName;

class  TDLDataExpression;
class   TDLDataTop;
class   TDLDataBottom;
class   TDLDataTypeName;
class   TDLDataValue;
class   TDLDataNot;
class   TDLDataAnd;
class   TDLDataOr;
class   TDLDataOneOf;

/// general visitor for DL expressions
class DLExpressionVisitor
{
public:		// visitor interface
	// concept expressions
	virtual void visit ( TDLConceptTop& expr ) = 0;
	virtual void visit ( TDLConceptBottom& expr ) = 0;
	virtual void visit ( TDLConceptName& expr ) = 0;
	virtual void visit ( TDLConceptNot& expr ) = 0;
	virtual void visit ( TDLConceptAnd& expr ) = 0;
	virtual void visit ( TDLConceptOr& expr ) = 0;
	virtual void visit ( TDLConceptOneOf& expr ) = 0;
	virtual void visit ( TDLConceptObjectSelf& expr ) = 0;
	virtual void visit ( TDLConceptObjectValue& expr ) = 0;
	virtual void visit ( TDLConceptObjectExists& expr ) = 0;
	virtual void visit ( TDLConceptObjectForall& expr ) = 0;
	virtual void visit ( TDLConceptObjectMinCardinality& expr ) = 0;
	virtual void visit ( TDLConceptObjectMaxCardinality& expr ) = 0;
	virtual void visit ( TDLConceptObjectExactCardinality& expr ) = 0;
	virtual void visit ( TDLConceptDataValue& expr ) = 0;
	virtual void visit ( TDLConceptDataExists& expr ) = 0;
	virtual void visit ( TDLConceptDataForall& expr ) = 0;
	virtual void visit ( TDLConceptDataMinCardinality& expr ) = 0;
	virtual void visit ( TDLConceptDataMaxCardinality& expr ) = 0;
	virtual void visit ( TDLConceptDataExactCardinality& expr ) = 0;

	// individual expressions
	virtual void visit ( TDLIndividualName& expr ) = 0;

	// object role expressions
	virtual void visit ( TDLObjectRoleTop& expr ) = 0;
	virtual void visit ( TDLObjectRoleBottom& expr ) = 0;
	virtual void visit ( TDLObjectRoleName& expr ) = 0;
	virtual void visit ( TDLObjectRoleInverse& expr ) = 0;
	virtual void visit ( TDLObjectRoleChain& expr ) = 0;
	virtual void visit ( TDLObjectRoleProjectionFrom& expr ) = 0;
	virtual void visit ( TDLObjectRoleProjectionInto& expr ) = 0;

	// data role expressions
	virtual void visit ( TDLDataRoleTop& expr ) = 0;
	virtual void visit ( TDLDataRoleBottom& expr ) = 0;
	virtual void visit ( TDLDataRoleName& expr ) = 0;

	// data expressions
	virtual void visit ( TDLDataTop& expr ) = 0;
	virtual void visit ( TDLDataBottom& expr ) = 0;
	virtual void visit ( TDLDataTypeName& expr ) = 0;
	virtual void visit ( TDLDataValue& expr ) = 0;
	virtual void visit ( TDLDataNot& expr ) = 0;
	virtual void visit ( TDLDataAnd& expr ) = 0;
	virtual void visit ( TDLDataOr& expr ) = 0;
	virtual void visit ( TDLDataOneOf& expr ) = 0;

	// other methods
	virtual ~DLExpressionVisitor ( void ) {}
}; // DLExpressionVisitor


/// base class for the DL expression, which include concept-, (data)role-, individual-, and data ones
class TDLExpression
{
public:		// interface
		/// empty c'tor
	TDLExpression ( void ) {}
		/// empty d'tor: note that no deep delete is necessary as all the elements are RO
	virtual ~TDLExpression ( void ) {}

		/// accept method for the visitor pattern
	virtual void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLExpression


//------------------------------------------------------------------
//	helper classes
//------------------------------------------------------------------


//------------------------------------------------------------------
///	named entity
//------------------------------------------------------------------
class TNamedEntity
{
protected:	// members
		/// name of the entity
	std::string Name;

public:		// interface
		/// c'tor: initialise name
	TNamedEntity ( const std::string& name ) : Name(name) {}
		/// empty d'tor
	virtual ~TNamedEntity ( void ) {}

		/// get access to the name
	const char* getName ( void ) const { return Name.c_str(); }
}; // TNamedEntity

//------------------------------------------------------------------
///	concept argument
//------------------------------------------------------------------
class TConceptArg
{
protected:	// members
		/// concept argument
	TDLConceptExpression* C;

public:		// interface
		/// init c'tor
	TConceptArg ( TDLConceptExpression* c ) : C(c) {}
		/// empty d'tor
	virtual ~TConceptArg ( void ) {}

		/// get access to the argument
	TDLConceptExpression* getC ( void ) const { return C; }
}; // TConceptArg

//------------------------------------------------------------------
///	individual argument
//------------------------------------------------------------------
class TIndividualArg
{
protected:	// members
		/// individual argument
	TDLIndividualExpression* I;

public:		// interface
		/// init c'tor
	TIndividualArg ( TDLIndividualExpression* i ) : I(i) {}
		/// empty d'tor
	virtual ~TIndividualArg ( void ) {}

		/// get access to the argument
	TDLIndividualExpression* getI ( void ) const { return I; }
}; // TIndividualArg

//------------------------------------------------------------------
///	numerical argument
//------------------------------------------------------------------
class TNumberArg
{
protected:	// members
		/// number argument
	unsigned int N;

public:		// interface
		/// init c'tor
	TNumberArg ( unsigned int n ) : N(n) {}
		/// empty d'tor
	virtual ~TNumberArg ( void ) {}

		/// get access to the argument
	unsigned int getNumber ( void ) const { return N; }
}; // TNumberArg

//------------------------------------------------------------------
///	object role argument
//------------------------------------------------------------------
class TObjectRoleArg
{
protected:	// members
		/// object role argument
	TDLObjectRoleExpression* OR;

public:		// interface
		/// init c'tor
	TObjectRoleArg ( TDLObjectRoleExpression* oR ) : OR(oR) {}
		/// empty d'tor
	virtual ~TObjectRoleArg ( void ) {}

		/// get access to the argument
	TDLObjectRoleExpression* getOR ( void ) const { return OR; }
}; // TObjectRoleArg

//------------------------------------------------------------------
///	data role argument
//------------------------------------------------------------------
class TDataRoleArg
{
protected:	// members
		/// data role argument
	TDLDataRoleExpression* DR;

public:		// interface
		/// init c'tor
	TDataRoleArg ( TDLDataRoleExpression* dR ) : DR(dR) {}
		/// empty d'tor
	virtual ~TDataRoleArg ( void ) {}

		/// get access to the argument
	TDLDataRoleExpression* getDR ( void ) const { return DR; }
}; // TDataRoleArg

//------------------------------------------------------------------
///	data expression argument (templated with the exact type)
//------------------------------------------------------------------
template<class TExpression>
class TDataExpressionArg
{
protected:	// members
		/// data expression argument
	TExpression* Expr;

public:		// interface
		/// init c'tor
	TDataExpressionArg ( TExpression* expr ) : Expr(expr) {}
		/// empty d'tor
	virtual ~TDataExpressionArg ( void ) {}

		/// get access to the argument
	TExpression* getExpr ( void ) const { return Expr; }
}; // TDataExpressionArg

//------------------------------------------------------------------
///	general n-argument expression
//------------------------------------------------------------------
template<class Argument>
class TDLNAryExpression
{
public:		// types
		/// base type
	typedef std::vector<Argument*> ArgumentArray;
		/// RW iterator over base type
	typedef typename ArgumentArray::const_iterator iterator;
		/// input array type
	typedef std::vector<TDLExpression*> ExpressionArray;
		/// RW input iterator
	typedef ExpressionArray::const_iterator i_iterator;

protected:	// members
		/// set of equivalent concept descriptions
	ArgumentArray Base;
		/// name for excepion depending on class name and direction
	std::string EString;

protected:	// methods
		/// transform general expression into the argument one
	Argument* transform ( TDLExpression* arg )
	{
		Argument* p = dynamic_cast<Argument*>(arg);
		if ( p == NULL )
			throw EFaCTPlusPlus(EString.c_str());
		return p;
	}

public:		// interface
		/// c'tor: build an error string
	TDLNAryExpression ( const char* typeName, const char* className )
	{
		EString = "Expected ";
		EString += typeName;
		EString += " argument in the '";
		EString += className;
		EString += "' expression";
	}
		/// empty d'tor
	virtual ~TDLNAryExpression ( void ) {}

	// add elements to the array

		/// add a single element to the array
	void add ( TDLExpression* p ) { Base.push_back(transform(p)); }
		/// add a range to the array
	void add ( i_iterator b, i_iterator e )
	{
		for ( ; b != e; ++b )
			add(*b);
	}
		/// add a vector
	void add ( const ExpressionArray& v ) { add ( v.begin(), v.end() ); }

	// access to members

		/// RW begin iterator for array
	iterator begin ( void ) { return Base.begin(); }
		/// RW end iterator for array
	iterator end ( void ) { return Base.end(); }
}; // TDLNAryExpression


//------------------------------------------------------------------
//	concept expressions
//------------------------------------------------------------------


//------------------------------------------------------------------
///	general concept expression
//------------------------------------------------------------------
class TDLConceptExpression: public TDLExpression
{
public:		// interface
		/// empty c'tor
	TDLConceptExpression ( void ) : TDLExpression() {}
		/// empty d'tor
	virtual ~TDLConceptExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLConceptExpression

//------------------------------------------------------------------
///	concept TOP expression
//------------------------------------------------------------------
class TDLConceptTop: public TDLConceptExpression
{
public:		// interface
		/// empty c'tor
	TDLConceptTop ( void ) : TDLConceptExpression() {}
		/// empty d'tor
	virtual ~TDLConceptTop ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptTop

//------------------------------------------------------------------
///	concept BOTTOM expression
//------------------------------------------------------------------
class TDLConceptBottom: public TDLConceptExpression
{
public:		// interface
		/// empty c'tor
	TDLConceptBottom ( void ) : TDLConceptExpression() {}
		/// empty d'tor
	virtual ~TDLConceptBottom ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptBottom

//------------------------------------------------------------------
///	named concept expression
//------------------------------------------------------------------
class TDLConceptName: public TDLConceptExpression, public TNamedEntity
{
public:		// interface
		/// init c'tor
	TDLConceptName ( const std::string& name ) : TDLConceptExpression(), TNamedEntity(name) {}
		/// empty d'tor
	virtual ~TDLConceptName ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptName

//------------------------------------------------------------------
///	concept NOT expression
//------------------------------------------------------------------
class TDLConceptNot: public TDLConceptExpression, public TConceptArg
{
public:		// interface
		/// init c'tor
	TDLConceptNot ( TDLConceptExpression* C )
		: TDLConceptExpression()
		, TConceptArg(C)
		{}
		/// empty d'tor
	virtual ~TDLConceptNot ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptNot

//------------------------------------------------------------------
///	concept AND expression
//------------------------------------------------------------------
class TDLConceptAnd: public TDLConceptExpression, public TDLNAryExpression<TDLConceptExpression>
{
public:		// interface
		/// init c'tor: create AND of expressions from the given array
	TDLConceptAnd ( const ExpressionArray& v )
		: TDLConceptExpression()
		, TDLNAryExpression<TDLConceptExpression>("concept expression","AND")
	{
		add(v);
	}
		/// empty d'tor
	virtual ~TDLConceptAnd ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptAnd

//------------------------------------------------------------------
///	concept OR expression
//------------------------------------------------------------------
class TDLConceptOr: public TDLConceptExpression, public TDLNAryExpression<TDLConceptExpression>
{
public:		// interface
		/// init c'tor: create OR of expressions from the given array
	TDLConceptOr ( const ExpressionArray& v )
		: TDLConceptExpression()
		, TDLNAryExpression<TDLConceptExpression>("concept expression","OR")
	{
		add(v);
	}
		/// empty d'tor
	virtual ~TDLConceptOr ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptOr

//------------------------------------------------------------------
///	concept one-of expression
//------------------------------------------------------------------
class TDLConceptOneOf: public TDLConceptExpression, public TDLNAryExpression<TDLIndividualName>
{
public:		// interface
		/// init c'tor: create one-of from individuals in the given array
	TDLConceptOneOf ( const ExpressionArray& v )
		: TDLConceptExpression()
		, TDLNAryExpression<TDLIndividualName>("individual name","OneOf")
	{
		add(v);
	}
		/// empty d'tor
	virtual ~TDLConceptOneOf ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptOneOf

//------------------------------------------------------------------
///	general concept expression that contains an object role
//------------------------------------------------------------------
class TDLConceptObjectRoleExpression: public TDLConceptExpression, public TObjectRoleArg
{
public:		// interface
		/// init c'tor
	TDLConceptObjectRoleExpression ( TDLObjectRoleExpression* R )
		: TDLConceptExpression()
		, TObjectRoleArg(R)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectRoleExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLConceptObjectRoleExpression

//------------------------------------------------------------------
///	concept self-ref expression
//------------------------------------------------------------------
class TDLConceptObjectSelf: public TDLConceptObjectRoleExpression
{
public:		// interface
		/// init c'tor
	TDLConceptObjectSelf ( TDLObjectRoleExpression* R )
		: TDLConceptObjectRoleExpression(R)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectSelf ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptObjectSelf

//------------------------------------------------------------------
///	concept some value restriction expression
//------------------------------------------------------------------
class TDLConceptObjectValue: public TDLConceptObjectRoleExpression, public TIndividualArg
{
public:		// interface
		/// init c'tor
	TDLConceptObjectValue ( TDLObjectRoleExpression* R, TDLIndividualExpression* I )
		: TDLConceptObjectRoleExpression(R)
		, TIndividualArg(I)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectValue ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptObjectValue

//------------------------------------------------------------------
///	general concept expression that contains an object role and a class expression
//------------------------------------------------------------------
class TDLConceptObjectRCExpression: public TDLConceptObjectRoleExpression, public TConceptArg
{
public:		// interface
		/// init c'tor
	TDLConceptObjectRCExpression ( TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLConceptObjectRoleExpression(R)
		, TConceptArg(C)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectRCExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLConceptObjectRCExpression

//------------------------------------------------------------------
///	concept object existential restriction expression
//------------------------------------------------------------------
class TDLConceptObjectExists: public TDLConceptObjectRCExpression
{
public:		// interface
		/// init c'tor
	TDLConceptObjectExists ( TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLConceptObjectRCExpression(R,C)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectExists ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptObjectExists

//------------------------------------------------------------------
///	concept object universal restriction expression
//------------------------------------------------------------------
class TDLConceptObjectForall: public TDLConceptObjectRCExpression
{
public:		// interface
		/// init c'tor
	TDLConceptObjectForall ( TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLConceptObjectRCExpression(R,C)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectForall ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptObjectForall

//------------------------------------------------------------------
///	general object role cardinality expression
//------------------------------------------------------------------
class TDLConceptObjectCardinalityExpression: public TDLConceptObjectRCExpression, public TNumberArg
{
public:		// interface
		/// init c'tor
	TDLConceptObjectCardinalityExpression ( unsigned int n, TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLConceptObjectRCExpression(R,C)
		, TNumberArg(n)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectCardinalityExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLConceptObjectCardinalityExpression

//------------------------------------------------------------------
///	concept object min cardinality expression
//------------------------------------------------------------------
class TDLConceptObjectMinCardinality: public TDLConceptObjectCardinalityExpression
{
public:		// interface
		/// init c'tor
	TDLConceptObjectMinCardinality ( unsigned int n, TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLConceptObjectCardinalityExpression(n,R,C)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectMinCardinality ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptObjectMinCardinality

//------------------------------------------------------------------
///	concept object max cardinality expression
//------------------------------------------------------------------
class TDLConceptObjectMaxCardinality: public TDLConceptObjectCardinalityExpression
{
public:		// interface
		/// init c'tor
	TDLConceptObjectMaxCardinality ( unsigned int n, TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLConceptObjectCardinalityExpression(n,R,C)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectMaxCardinality ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptObjectMaxCardinality

//------------------------------------------------------------------
///	concept object exact cardinality expression
//------------------------------------------------------------------
class TDLConceptObjectExactCardinality: public TDLConceptObjectCardinalityExpression
{
public:		// interface
		/// init c'tor
	TDLConceptObjectExactCardinality ( unsigned int n, TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLConceptObjectCardinalityExpression(n,R,C)
		{}
		/// empty d'tor
	virtual ~TDLConceptObjectExactCardinality ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptObjectExactCardinality

//------------------------------------------------------------------
///	general concept expression that contains an data role
//------------------------------------------------------------------
class TDLConceptDataRoleExpression: public TDLConceptExpression, public TDataRoleArg
{
public:		// interface
		/// init c'tor
	TDLConceptDataRoleExpression ( TDLDataRoleExpression* R )
		: TDLConceptExpression()
		, TDataRoleArg(R)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataRoleExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLConceptDataRoleExpression

//------------------------------------------------------------------
///	concept some value restriction expression
//------------------------------------------------------------------
class TDLConceptDataValue: public TDLConceptDataRoleExpression, public TDataExpressionArg<TDLDataValue>
{
public:		// interface
		/// init c'tor
	TDLConceptDataValue ( TDLDataRoleExpression* R, TDLDataValue* V )
		: TDLConceptDataRoleExpression(R)
		, TDataExpressionArg<TDLDataValue>(V)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataValue ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptDataValue

//------------------------------------------------------------------
///	general concept expression that contains an data role and a data expression
//------------------------------------------------------------------
class TDLConceptDataRVExpression: public TDLConceptDataRoleExpression, public TDataExpressionArg<TDLDataExpression>
{
public:		// interface
		/// init c'tor
	TDLConceptDataRVExpression ( TDLDataRoleExpression* R, TDLDataExpression* E )
		: TDLConceptDataRoleExpression(R)
		, TDataExpressionArg<TDLDataExpression>(E)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataRVExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLConceptDataRVExpression

//------------------------------------------------------------------
///	concept data existential restriction expression
//------------------------------------------------------------------
class TDLConceptDataExists: public TDLConceptDataRVExpression
{
public:		// interface
		/// init c'tor
	TDLConceptDataExists ( TDLDataRoleExpression* R, TDLDataExpression* E )
		: TDLConceptDataRVExpression(R,E)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataExists ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptDataExists

//------------------------------------------------------------------
///	concept data universal restriction expression
//------------------------------------------------------------------
class TDLConceptDataForall: public TDLConceptDataRVExpression
{
public:		// interface
		/// init c'tor
	TDLConceptDataForall ( TDLDataRoleExpression* R, TDLDataExpression* E )
		: TDLConceptDataRVExpression(R,E)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataForall ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptDataForall

//------------------------------------------------------------------
///	general data role cardinality expression
//------------------------------------------------------------------
class TDLConceptDataCardinalityExpression: public TDLConceptDataRVExpression, public TNumberArg
{
public:		// interface
		/// init c'tor
	TDLConceptDataCardinalityExpression ( unsigned int n, TDLDataRoleExpression* R, TDLDataExpression* E )
		: TDLConceptDataRVExpression(R,E)
		, TNumberArg(n)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataCardinalityExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLConceptDataCardinalityExpression

//------------------------------------------------------------------
///	concept data min cardinality expression
//------------------------------------------------------------------
class TDLConceptDataMinCardinality: public TDLConceptDataCardinalityExpression
{
public:		// interface
		/// init c'tor
	TDLConceptDataMinCardinality ( unsigned int n, TDLDataRoleExpression* R, TDLDataExpression* E )
		: TDLConceptDataCardinalityExpression(n,R,E)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataMinCardinality ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptDataMinCardinality

//------------------------------------------------------------------
///	concept data max cardinality expression
//------------------------------------------------------------------
class TDLConceptDataMaxCardinality: public TDLConceptDataCardinalityExpression
{
public:		// interface
		/// init c'tor
	TDLConceptDataMaxCardinality ( unsigned int n, TDLDataRoleExpression* R, TDLDataExpression* E )
		: TDLConceptDataCardinalityExpression(n,R,E)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataMaxCardinality ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptDataMaxCardinality

//------------------------------------------------------------------
///	concept data exact cardinality expression
//------------------------------------------------------------------
class TDLConceptDataExactCardinality: public TDLConceptDataCardinalityExpression
{
public:		// interface
		/// init c'tor
	TDLConceptDataExactCardinality ( unsigned int n, TDLDataRoleExpression* R, TDLDataExpression* E )
		: TDLConceptDataCardinalityExpression(n,R,E)
		{}
		/// empty d'tor
	virtual ~TDLConceptDataExactCardinality ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLConceptDataExactCardinality


//------------------------------------------------------------------
//	individual expressions
//------------------------------------------------------------------


//------------------------------------------------------------------
///	general individual expression
//------------------------------------------------------------------
class TDLIndividualExpression: public TDLExpression
{
public:		// interface
		/// empty c'tor
	TDLIndividualExpression ( void ) : TDLExpression() {}
		/// empty d'tor
	virtual ~TDLIndividualExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLIndividualExpression

//------------------------------------------------------------------
///	named individual expression
//------------------------------------------------------------------
class TDLIndividualName: public TDLIndividualExpression, public TNamedEntity
{
public:		// interface
		/// init c'tor
	TDLIndividualName ( const std::string& name ) : TDLIndividualExpression(), TNamedEntity(name) {}
		/// empty d'tor
	virtual ~TDLIndividualName ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLIndividualName


//------------------------------------------------------------------
//	role expressions
//------------------------------------------------------------------


//------------------------------------------------------------------
///	general role expression
//------------------------------------------------------------------
class TDLRoleExpression: public TDLExpression
{
public:		// interface
		/// empty c'tor
	TDLRoleExpression ( void ) : TDLExpression() {}
		/// empty d'tor
	virtual ~TDLRoleExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLRoleExpression


//------------------------------------------------------------------
//	object role expressions
//------------------------------------------------------------------


//------------------------------------------------------------------
///	complex object role expression (general expression, role chain or projection)
//------------------------------------------------------------------
class TDLObjectRoleComplexExpression: public TDLRoleExpression
{
public:		// interface
		/// empty c'tor
	TDLObjectRoleComplexExpression ( void ) : TDLRoleExpression() {}
		/// empty d'tor
	virtual ~TDLObjectRoleComplexExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLObjectRoleComplexExpression

//------------------------------------------------------------------
///	general object role expression
//------------------------------------------------------------------
class TDLObjectRoleExpression: public TDLObjectRoleComplexExpression
{
public:		// interface
		/// empty c'tor
	TDLObjectRoleExpression ( void ) : TDLObjectRoleComplexExpression() {}
		/// empty d'tor
	virtual ~TDLObjectRoleExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLObjectRoleExpression

//------------------------------------------------------------------
///	object role TOP expression
//------------------------------------------------------------------
class TDLObjectRoleTop: public TDLObjectRoleExpression
{
public:		// interface
		/// empty c'tor
	TDLObjectRoleTop ( void ) : TDLObjectRoleExpression() {}
		/// empty d'tor
	virtual ~TDLObjectRoleTop ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLObjectRoleTop

//------------------------------------------------------------------
///	object role BOTTOM expression
//------------------------------------------------------------------
class TDLObjectRoleBottom: public TDLObjectRoleExpression
{
public:		// interface
		/// empty c'tor
	TDLObjectRoleBottom ( void ) : TDLObjectRoleExpression() {}
		/// empty d'tor
	virtual ~TDLObjectRoleBottom ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLObjectRoleBottom

//------------------------------------------------------------------
///	named object role expression
//------------------------------------------------------------------
class TDLObjectRoleName: public TDLObjectRoleExpression, public TNamedEntity
{
public:		// interface
		/// init c'tor
	TDLObjectRoleName ( const std::string& name ) : TDLObjectRoleExpression(), TNamedEntity(name) {}
		/// empty d'tor
	virtual ~TDLObjectRoleName ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLObjectRoleName

//------------------------------------------------------------------
///	inverse object role expression
//------------------------------------------------------------------
class TDLObjectRoleInverse: public TDLObjectRoleExpression, public TObjectRoleArg
{
public:		// interface
		/// init c'tor
	TDLObjectRoleInverse ( TDLObjectRoleExpression* R )
		: TDLObjectRoleExpression()
		, TObjectRoleArg(R)
		{}
		/// empty d'tor
	virtual ~TDLObjectRoleInverse ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLObjectRoleInverse

//------------------------------------------------------------------
/// object role chain expression
//------------------------------------------------------------------
class TDLObjectRoleChain: public TDLObjectRoleComplexExpression, public TDLNAryExpression<TDLObjectRoleExpression>
{
public:		// interface
		/// init c'tor: create role chain from given array
	TDLObjectRoleChain ( const ExpressionArray& v )
		: TDLObjectRoleComplexExpression()
		, TDLNAryExpression<TDLObjectRoleExpression>("object role expression","role chain")
	{
		add(v);
	}
		/// empty d'tor
	virtual ~TDLObjectRoleChain ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLObjectRoleChain

//------------------------------------------------------------------
///	object role projection from expression
//------------------------------------------------------------------
class TDLObjectRoleProjectionFrom
	: public TDLObjectRoleComplexExpression
	, public TObjectRoleArg
	, public TConceptArg
{
public:		// interface
		/// init c'tor
	TDLObjectRoleProjectionFrom ( TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLObjectRoleComplexExpression()
		, TObjectRoleArg(R)
		, TConceptArg(C)
		{}
		/// empty d'tor
	virtual ~TDLObjectRoleProjectionFrom ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLObjectRoleProjectionFrom

//------------------------------------------------------------------
///	object role projection from expression
//------------------------------------------------------------------
class TDLObjectRoleProjectionInto
	: public TDLObjectRoleComplexExpression
	, public TObjectRoleArg
	, public TConceptArg
{
public:		// interface
		/// init c'tor
	TDLObjectRoleProjectionInto ( TDLObjectRoleExpression* R, TDLConceptExpression* C )
		: TDLObjectRoleComplexExpression()
		, TObjectRoleArg(R)
		, TConceptArg(C)
		{}
		/// empty d'tor
	virtual ~TDLObjectRoleProjectionInto ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLObjectRoleProjectionInto


//------------------------------------------------------------------
//	data role expressions
//------------------------------------------------------------------


//------------------------------------------------------------------
///	general data role expression
//------------------------------------------------------------------
class TDLDataRoleExpression: public TDLRoleExpression
{
public:		// interface
		/// empty c'tor
	TDLDataRoleExpression ( void ) : TDLRoleExpression() {}
		/// empty d'tor
	virtual ~TDLDataRoleExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLDataRoleExpression

//------------------------------------------------------------------
///	data role TOP expression
//------------------------------------------------------------------
class TDLDataRoleTop: public TDLDataRoleExpression
{
public:		// interface
		/// empty c'tor
	TDLDataRoleTop ( void ) : TDLDataRoleExpression() {}
		/// empty d'tor
	virtual ~TDLDataRoleTop ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataRoleTop

//------------------------------------------------------------------
///	data role BOTTOM expression
//------------------------------------------------------------------
class TDLDataRoleBottom: public TDLDataRoleExpression
{
public:		// interface
		/// empty c'tor
	TDLDataRoleBottom ( void ) : TDLDataRoleExpression() {}
		/// empty d'tor
	virtual ~TDLDataRoleBottom ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataRoleBottom

//------------------------------------------------------------------
///	named data role expression
//------------------------------------------------------------------
class TDLDataRoleName: public TDLDataRoleExpression, public TNamedEntity
{
public:		// interface
		/// init c'tor
	TDLDataRoleName ( const std::string& name ) : TDLDataRoleExpression(), TNamedEntity(name) {}
		/// empty d'tor
	virtual ~TDLDataRoleName ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataRoleName


//------------------------------------------------------------------
//	data expressions
//------------------------------------------------------------------


//------------------------------------------------------------------
///	general data expression
//------------------------------------------------------------------
class TDLDataExpression: public TDLExpression
{
public:		// interface
		/// empty c'tor
	TDLDataExpression ( void ) : TDLExpression() {}
		/// empty d'tor
	virtual ~TDLDataExpression ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) = 0;
}; // TDLDataExpression

//------------------------------------------------------------------
///	data TOP expression
//------------------------------------------------------------------
class TDLDataTop: public TDLDataExpression
{
public:		// interface
		/// empty c'tor
	TDLDataTop ( void ) : TDLDataExpression() {}
		/// empty d'tor
	virtual ~TDLDataTop ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataTop

//------------------------------------------------------------------
///	data BOTTOM expression
//------------------------------------------------------------------
class TDLDataBottom: public TDLDataExpression
{
public:		// interface
		/// empty c'tor
	TDLDataBottom ( void ) : TDLDataExpression() {}
		/// empty d'tor
	virtual ~TDLDataBottom ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataBottom

//------------------------------------------------------------------
///	named data type expression
//------------------------------------------------------------------
class TDLDataTypeName: public TDLDataExpression, public TNamedEntity
{
public:		// interface
		/// init c'tor
	TDLDataTypeName ( const std::string& name ) : TDLDataExpression(), TNamedEntity(name) {}
		/// empty d'tor
	virtual ~TDLDataTypeName ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataTypeName

//------------------------------------------------------------------
///	data value expression
//------------------------------------------------------------------
class TDLDataValue: public TDLDataExpression, public TNamedEntity, public TDataExpressionArg<TDLDataTypeName>
{
public:		// interface
		/// init c'tor
	TDLDataValue ( const std::string& value, TDLDataTypeName* T )
		: TDLDataExpression()
		, TNamedEntity(value)
		, TDataExpressionArg<TDLDataTypeName>(T)
		{}
		/// empty d'tor
	virtual ~TDLDataValue ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataValue

//------------------------------------------------------------------
///	data NOT expression
//------------------------------------------------------------------
class TDLDataNot: public TDLDataExpression, public TDataExpressionArg<TDLDataExpression>
{
public:		// interface
		/// init c'tor
	TDLDataNot ( TDLDataExpression* E )
		: TDLDataExpression()
		, TDataExpressionArg<TDLDataExpression>(E)
		{}
		/// empty d'tor
	virtual ~TDLDataNot ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataNot

//------------------------------------------------------------------
///	data AND expression
//------------------------------------------------------------------
class TDLDataAnd: public TDLDataExpression, public TDLNAryExpression<TDLDataExpression>
{
public:		// interface
		/// init c'tor: create AND of expressions from the given array
	TDLDataAnd ( const ExpressionArray& v )
		: TDLDataExpression()
		, TDLNAryExpression<TDLDataExpression>("data expression","data AND")
	{
		add(v);
	}
		/// empty d'tor
	virtual ~TDLDataAnd ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataAnd

//------------------------------------------------------------------
///	data OR expression
//------------------------------------------------------------------
class TDLDataOr: public TDLDataExpression, public TDLNAryExpression<TDLDataExpression>
{
public:		// interface
		/// init c'tor: create OR of expressions from the given array
	TDLDataOr ( const ExpressionArray& v )
		: TDLDataExpression()
		, TDLNAryExpression<TDLDataExpression>("data expression","data OR")
	{
		add(v);
	}
		/// empty d'tor
	virtual ~TDLDataOr ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataOr

//------------------------------------------------------------------
///	data one-of expression
//------------------------------------------------------------------
class TDLDataOneOf: public TDLDataExpression, public TDLNAryExpression<TDLDataValue>
{
public:		// interface
		/// init c'tor: create one-of from individuals in the given array
	TDLDataOneOf ( const ExpressionArray& v )
		: TDLDataExpression()
		, TDLNAryExpression<TDLDataValue>("data value","data OneOf")
	{
		add(v);
	}
		/// empty d'tor
	virtual ~TDLDataOneOf ( void ) {}

		/// accept method for the visitor pattern
	void accept ( DLExpressionVisitor& visitor ) { visitor.visit(*this); }
}; // TDLDataOneOf

#endif