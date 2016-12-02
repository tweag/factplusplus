module HFact.C.Types where

import Foreign.ForeignPtr
import Foreign.Ptr

type ReasoningKernel = ForeignPtr CReasoningKernel
newtype CReasoningKernel = CReasoningKernel (Ptr CReasoningKernel)

type Actor = ForeignPtr CActor
newtype CActor = CActor (Ptr CActor)

-- TODO: (Maybe) implement concept_free
type ConceptExpression = CConceptExpression
newtype CConceptExpression = CConceptExpression (Ptr CConceptExpression)

type ObjectRoleExpression = CObjectRoleExpression
newtype CObjectRoleExpression = CObjectRoleExpression (Ptr CObjectRoleExpression)

type IndividualExpression = CIndividualExpression
newtype CIndividualExpression = CIndividualExpression (Ptr CIndividualExpression)

type Axiom = CAxiom
newtype CAxiom = CAxiom (Ptr CAxiom)

type DataRoleExpression = CDataRoleExpression
newtype CDataRoleExpression = CDataRoleExpression (Ptr CDataRoleExpression)

type DataValueExpression = CDataValueExpression
newtype CDataValueExpression = CDataValueExpression (Ptr CDataValueExpression)

type DataTypeExpression = CDataTypeExpression
newtype CDataTypeExpression = CDataTypeExpression (Ptr CDataTypeExpression)

data RelativesInfo = Direct | All
