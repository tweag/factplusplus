/* This file is part of the FaCT++ DL reasoner
Copyright (C) 2003-2015 Dmitry Tsarkov and The University of Manchester
Copyright (C) 2015-2016 Dmitry Tsarkov

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

#ifndef DLCONCEPTTAXONOMY_H
#define DLCONCEPTTAXONOMY_H

#include "TaxonomyCreator.h"
#include "dlTBox.h"
#include "tProgressMonitor.h"

/// Taxonomy of named DL concepts (and mapped individuals)
class DLConceptTaxonomy: public TaxonomyCreator
{
protected:	// types
	typedef std::vector<TaxonomyVertex*> TaxVertexVec;
		/// all the derived subsumers of a class (came from the model)
	class DerivedSubsumers: public KnownSubsumers
	{
	protected:	// typedefs
			/// set of the subsumers
		typedef TaxonomyCreator::SubsumerSet SubsumerSet;
			/// SS RW iterator
		typedef SubsumerSet::iterator ss_iterator;

	protected:	// members
			/// set of sure- and possible subsumers
		SubsumerSet Sure, Possible;

	public:		// interface
			/// c'tor: copy given sets
		DerivedSubsumers ( const SubsumerSet& sure, const SubsumerSet& possible )
			: KnownSubsumers()
			, Sure(sure)
			, Possible(possible)
			{}
			/// empty d'tor
		virtual ~DerivedSubsumers ( void ) {}

		// iterators

			/// begin of the Sure subsumers interval
		virtual ss_iterator s_begin ( void ) { return Sure.begin(); }
			/// end of the Sure subsumers interval
		virtual ss_iterator s_end ( void ) { return Sure.end(); }
			/// begin of the Possible subsumers interval
		virtual ss_iterator p_begin ( void ) { return Possible.begin(); }
			/// end of the Possible subsumers interval
		virtual ss_iterator p_end ( void ) { return Possible.end(); }
	}; // DerivedSubsumers

protected:	// members
		/// host tBox
	TBox& tBox;
		/// incremental sets M+ and M-
	std::set<const TNamedEntity*> MPlus, MMinus;
		/// set of possible parents
	std::set<TaxonomyVertex*> candidates;
		/// whether look into it
	bool useCandidates;
		/// common descendants of all parents of currently classified concept
	TaxVertexVec Common;
		/// number of processed common parents
	unsigned int nCommon;

	// statistic counters
	unsigned long nConcepts;
	unsigned long nTries;
	unsigned long nPositives;
	unsigned long nNegatives;
	unsigned long nSearchCalls;
	unsigned long nSubCalls;
	unsigned long nNonTrivialSubCalls;

		/// number of positive cached subsumptions
	unsigned long nCachedPositive;
		/// number of negative cached subsumptions
	unsigned long nCachedNegative;
		/// number of non-subsumptions detected by a sorted reasoning
	unsigned long nSortedNegative;
		/// number of non-subsumptions because of module reasons
	unsigned long nModuleNegative;

		/// indicator of taxonomy creation progress
	TProgressMonitor* pTaxProgress;

	// flags

		/// flag to use Bottom-Up search
	bool flagNeedBottomUp;

protected:	// methods
	//-----------------------------------------------------------------
	//--	General support for DL concept classification
	//-----------------------------------------------------------------

		/// get access to curEntry as a TConcept
	const TConcept* curConcept ( void ) const { return static_cast<const TConcept*>(curEntry); }
		/// tests subsumption (via tBox) and gather statistics.  Use cache and other optimisations.
	bool testSub ( const TConcept* p, const TConcept* q );
		/// test subsumption via TBox explicitly
	bool testSubTBox ( const TConcept* p, const TConcept* q )
	{
		bool res = tBox.isSubHolds ( p, q );

		// update statistic
		++nTries;

		if ( res )
			++nPositives;
		else
			++nNegatives;

		return res;
	}

	// interface from BAADER paper

		/// SEARCH procedure from Baader et al paper
	void searchBaader ( TaxonomyVertex* cur );
		/// ENHANCED_SUBS procedure from Baader et al paper
	bool enhancedSubs1 ( TaxonomyVertex* cur );
		/// short-cut from ENHANCED_SUBS
	bool enhancedSubs2 ( TaxonomyVertex* cur )
	{
		// if bottom-up search and CUR is not a successor of checking entity -- return false
		if ( unlikely(upDirection && !cur->isCommon()) )
			return false;
		if ( unlikely ( useCandidates && candidates.find(cur) == candidates.end() ) )
			return false;
		return enhancedSubs1(cur);
	}
		// wrapper for the ENHANCED_SUBS
	inline bool enhancedSubs ( TaxonomyVertex* cur )
	{
		++nSubCalls;

		if ( isValued(cur) )
			return getValue(cur);
		else
			return setValue ( cur, enhancedSubs2(cur) );
	}
		/// explicitly test appropriate subsumption relation
	bool testSubsumption ( TaxonomyVertex* cur );
		/// test whether a node could be a super-node of CUR
	bool possibleSub ( TaxonomyVertex* v ) const
	{
		const TConcept* C = static_cast<const TConcept*>(v->getPrimer());
		// non-prim concepts are candidates
		if ( !C->isPrimitive() )
			return true;
		// all others should be in the possible sups list
		return ksStack.top()->isPossibleSub(C);
	}
		/// @return true if non-subsumption is due to ENTITY is not in the \bot-module
	bool isNotInModule ( const TNamedEntity* entity ) const;
		/// reclassify node
	void reclassify ( TaxonomyVertex* node, const TSignature* s );

		/// propagate common value from NODE to all its descendants; save visited nodes
	void propagateOneCommon ( TaxonomyVertex* node );
		/// mark as COMMON all vertexes that are sub-concepts of every parent of CURRENT
	bool propagateUp ( void );
		/// clear all COMMON information
	void clearCommon ( void )
	{
		for ( TaxVertexVec::iterator p = Common.begin(), p_end = Common.end(); p < p_end; ++p )
			(*p)->clearCommon();
		Common.clear();
	}
		/// check if concept is unsat; add it as a synonym of BOTTOM if necessary
	bool isUnsatisfiable ( void );
		/// fill candidates
	void fillCandidates ( TaxonomyVertex* cur );

	//-----------------------------------------------------------------
	//--	Tunable methods (depending on taxonomy type)
	//-----------------------------------------------------------------

		/// prepare told subsumers for given entry if necessary
	virtual KnownSubsumers* buildKnownSubsumers ( ClassifiableEntry* p );
		/// prepare signature for given entry
	virtual TSignature* buildSignature ( ClassifiableEntry* p );
		/// check if no classification needed (synonym, orphan, unsatisfiable)
	virtual bool immediatelyClassified ( void );
		/// check if no BU classification is required as C=TOP
	bool isEqualToTop ( void );

		/// check if it is possible to skip TD phase
	virtual bool needTopDown ( void ) const
		{ return !(useCompletelyDefined && curEntry->isCompletelyDefined ()); }
		/// explicitly run TD phase
	virtual void runTopDown ( void ) { searchBaader(pTax->getTopVertex()); }
		/// check if it is possible to skip BU phase
	virtual bool needBottomUp ( void ) const
	{
		// we DON'T need bottom-up phase for primitive concepts during CD-like reasoning
		// if no GCIs are in the TBox (C [= T, T [= X or Y, X [= D, Y [= D) or (T [= {o})
		// or no reflexive roles w/RnD present (Refl(R), Range(R)=D)
		return flagNeedBottomUp || !useCompletelyDefined || curConcept()->isNonPrimitive();
	}
		/// explicitly run BU phase
	virtual void runBottomUp ( void )
	{
		if ( propagateUp() )	// Common is set up here
			return;
		if ( isEqualToTop() )	// nothing to do
			return;
		if ( pTax->queryMode() )	// after classification -- bottom set up already
			searchBaader(pTax->getBottomVertex());
		else	// during classification -- have to find leaf nodes
			for ( TaxVertexVec::iterator p = Common.begin(), p_end = Common.end(); p < p_end; ++p )
				if ( (*p)->noNeighbours(/*upDirection=*/false) )
					searchBaader(*p);
	}

		/// actions that to be done BEFORE entry will be classified
	virtual void preClassificationActions ( void )
	{
		++nConcepts;
		if ( pTaxProgress != nullptr )
			pTaxProgress->nextClass();
	}
		/// @return true iff curEntry is classified as a synonym
	virtual bool classifySynonym ( void );

		/// check if it is necessary to log taxonomy action
	virtual bool needLogging ( void ) const { return true; }

public:		// interface
		/// the only c'tor
	DLConceptTaxonomy ( Taxonomy* tax, TBox& kb )
		: TaxonomyCreator(tax)
		, tBox(kb)
		, useCandidates(false)
		, nCommon(0)
		, nConcepts (0), nTries (0), nPositives (0), nNegatives (0)
		, nSearchCalls(0)
		, nSubCalls(0)
		, nNonTrivialSubCalls(0)
		, nCachedPositive(0)
		, nCachedNegative(0)
		, nSortedNegative(0)
		, nModuleNegative(0)
		, pTaxProgress(nullptr)
	{
	}
		/// no copy c'tor
	DLConceptTaxonomy ( const DLConceptTaxonomy& ) = delete;
		/// no assignment
	DLConceptTaxonomy& operator = ( const DLConceptTaxonomy& ) = delete;
		/// d'tor
	virtual ~DLConceptTaxonomy ( void ) {}

		/// set bottom-up flag
	void setBottomUp ( const TKBFlags& GCIs ) { flagNeedBottomUp = (GCIs.isGCI() || (GCIs.isReflexive() && GCIs.isRnD())); }
		/// reclassify taxonomy wrt changed sets
	void reclassify ( const std::set<const TNamedEntity*>& MPlus, const std::set<const TNamedEntity*>& MMinus );
		/// set progress indicator
	void setProgressIndicator ( TProgressMonitor* pMon ) { pTaxProgress = pMon; }
		/// output taxonomy to a stream
	virtual void print ( std::ostream& o ) const;
}; // DLConceptTaxonomy

//
// DLConceptTaxonomy implementation
//

inline bool DLConceptTaxonomy :: isUnsatisfiable ( void )
{
	const TConcept* p = curConcept();

	if ( tBox.isSatisfiable(p) )
		return false;

	pTax->addCurrentToSynonym(pTax->getBottomVertex());
	return true;
}

inline bool DLConceptTaxonomy :: immediatelyClassified ( void )
{
	if ( classifySynonym() )
		return true;

	if ( curConcept()->getClassTag() == cttTrueCompletelyDefined )
		return false;	// true CD concepts can not be unsat

	// after SAT testing plan would be implemented
	tBox.initCache(const_cast<TConcept*>(curConcept()));

	return isUnsatisfiable();
}

//-----------------------------------------------------------------------------
//--		implementation of taxonomy-related parts of TBox
//-----------------------------------------------------------------------------

inline void
TBox :: initTaxonomy ( void )
{
	pTax = new Taxonomy ( pTop, pBottom );
	pTaxCreator = new DLConceptTaxonomy ( pTax, *this );
}

inline void
TBox :: classifyEntry ( TConcept* entry )
{
	if ( unlikely(isBlockedInd(entry)) )
		classifyEntry(getBlockingInd(entry));	// make sure that the possible synonym is already classified
	if ( !entry->isClassified() )
		pTaxCreator->classifyEntry(entry);
}

inline void
TBox :: reclassify ( const std::set<const TNamedEntity*>& MPlus, const std::set<const TNamedEntity*>& MMinus )
{
	pTaxCreator->reclassify ( MPlus, MMinus );
	Status = kbRealised;	// FIXME!! check whether it is classified/realised
}

#endif // DLCONCEPTTAXONOMY_H
