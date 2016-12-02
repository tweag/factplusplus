module HFact
  ( module HFact.C.Types
  , module HFact) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import HFact.C.Types

foreign import ccall unsafe "fact.h fact_get_version" c_fact_get_version :: IO CString
foreign import ccall unsafe "fact.h fact_reasoning_kernel_new" c_fact_reasoning_kernel_new :: IO CReasoningKernel
foreign import ccall unsafe "fact.h &fact_reasoning_kernel_free" c_fact_reasoning_kernel_free :: FunPtr (Ptr CReasoningKernel -> IO ())

foreign import ccall unsafe "fact.h fact_is_kb_consistent" c_fact_is_kb_consistent :: CReasoningKernel -> IO CInt
foreign import ccall unsafe "fact.h fact_preprocess_kb" c_fact_preprocess_kb :: CReasoningKernel -> IO ()
foreign import ccall unsafe "fact.h fact_classify_kb" c_fact_classify_kb :: CReasoningKernel -> IO ()
foreign import ccall unsafe "fact.h fact_realise_kb" c_fact_realise_kb :: CReasoningKernel -> IO ()

foreign import ccall unsafe "fact.h fact_concept_actor_new" c_fact_concept_actor_new :: IO CActor
foreign import ccall unsafe "fact.h fact_individual_actor_new" c_fact_individual_actor_new :: IO CActor
foreign import ccall unsafe "fact.h fact_o_role_actor_new" c_fact_o_actor_new :: IO CActor
foreign import ccall unsafe "fact.h fact_d_role_actor_new" c_fact_d_actor_new :: IO CActor
foreign import ccall unsafe "fact.h &fact_actor_free" c_fact_actor_free :: FunPtr (Ptr CActor -> IO ())

foreign import ccall unsafe "fact.h fact_concept" c_fact_concept :: CReasoningKernel -> CString -> IO CConceptExpression
foreign import ccall unsafe "fact.h fact_object_role" c_fact_object_role :: CReasoningKernel -> CString -> IO CObjectRoleExpression
foreign import ccall unsafe "fact.h fact_individual" c_fact_individual :: CReasoningKernel -> CString -> IO CIndividualExpression
foreign import ccall unsafe "fact.h fact_data_role" c_fact_data_role :: CReasoningKernel -> CString -> IO CDataRoleExpression
foreign import ccall unsafe "fact.h fact_data_value" c_fact_data_value :: CReasoningKernel -> CString -> CDataTypeExpression -> IO CDataValueExpression
foreign import ccall unsafe "fact.h fact_data_type" c_fact_data_type :: CReasoningKernel -> CString -> IO CDataTypeExpression

foreign import ccall unsafe "fact.h fact_related_to" c_fact_related_to :: CReasoningKernel -> CIndividualExpression -> CObjectRoleExpression -> CIndividualExpression -> IO CAxiom
foreign import ccall unsafe "fact.h fact_value_of" c_fact_value_of :: CReasoningKernel -> CIndividualExpression -> CDataRoleExpression -> CDataValueExpression -> IO CAxiom
foreign import ccall unsafe "fact.h fact_instance_of" c_fact_instance_of :: CReasoningKernel -> CIndividualExpression -> CConceptExpression -> IO CAxiom

foreign import ccall unsafe "fact.h fact_get_sub_concepts" c_fact_get_sub_concepts :: CReasoningKernel -> CConceptExpression -> CInt -> Ptr (Ptr CActor) -> IO ()
foreign import ccall unsafe "fact.h fact_get_direct_instances" c_fact_get_instances :: CReasoningKernel -> CConceptExpression -> Ptr (Ptr CActor) -> IO ()
foreign import ccall unsafe "fact.h fact_get_elements_1d" c_fact_get_elements_1d :: CActor -> IO (Ptr CString)
foreign import ccall unsafe "fact.h fact_get_elements_2d" c_fact_get_elements_2d :: CActor -> IO (Ptr (Ptr CString))
foreign import ccall unsafe "fact.h fact_is_instance" c_fact_is_instance :: CReasoningKernel -> CIndividualExpression -> CConceptExpression -> IO CInt

foreign import ccall unsafe "fact.h fact_get_str_data_type" c_fact_get_str_data_type :: CReasoningKernel -> IO CDataTypeExpression
foreign import ccall unsafe "fact.h fact_get_int_data_type" c_fact_get_int_data_type :: CReasoningKernel -> IO CDataTypeExpression
foreign import ccall unsafe "fact.h fact_get_real_data_type" c_fact_get_real_data_type :: CReasoningKernel -> IO CDataTypeExpression
foreign import ccall unsafe "fact.h fact_get_bool_data_type" c_fact_get_bool_data_type :: CReasoningKernel -> IO CDataTypeExpression
foreign import ccall unsafe "fact.h fact_get_time_data_type" c_fact_get_time_data_type :: CReasoningKernel -> IO CDataTypeExpression

individual, mkIndividual :: ReasoningKernel -> String -> IO IndividualExpression
individual k str = do
  cStr <- newCString str
  withForeignPtr k $ \ptr ->
    c_fact_individual (CReasoningKernel ptr) cStr

mkIndividual = individual

dataRole, mkDataRole :: ReasoningKernel -> String -> IO DataRoleExpression
dataRole k str = do
  cStr <- newCString str
  withForeignPtr k $ \ptr ->
    c_fact_data_role (CReasoningKernel ptr) cStr

mkDataRole = dataRole

dataValue, mkDataValue :: ReasoningKernel -> String -> DataTypeExpression -> IO DataValueExpression
dataValue k str dt = do
  cStr <- newCString str
  withForeignPtr k $ \ptr ->
    c_fact_data_value (CReasoningKernel ptr) cStr dt

mkDataValue = dataValue

dataType, mkDataType :: ReasoningKernel -> String -> IO DataTypeExpression
dataType k str = do
  cStr <- newCString str
  withForeignPtr k $ \ptr ->
    c_fact_data_type (CReasoningKernel ptr) cStr

mkDataType = dataType

getStrDataType ::  ReasoningKernel -> IO DataTypeExpression
getStrDataType k =
  withForeignPtr k $ \ptr ->
    c_fact_get_str_data_type (CReasoningKernel ptr)

getIntDataType ::  ReasoningKernel -> IO DataTypeExpression
getIntDataType k =
  withForeignPtr k $ \ptr ->
    c_fact_get_int_data_type (CReasoningKernel ptr)

getRealDataType ::  ReasoningKernel -> IO DataTypeExpression
getRealDataType k =
  withForeignPtr k $ \ptr ->
    c_fact_get_real_data_type (CReasoningKernel ptr)

getBoolDataType ::  ReasoningKernel -> IO DataTypeExpression
getBoolDataType k =
  withForeignPtr k $ \ptr ->
    c_fact_get_bool_data_type (CReasoningKernel ptr)

getTimeDataType ::  ReasoningKernel -> IO DataTypeExpression
getTimeDataType k =
  withForeignPtr k $ \ptr ->
    c_fact_get_time_data_type (CReasoningKernel ptr)

getVersion :: IO String
getVersion = peekCString =<< c_fact_get_version

newReasoningKernel :: IO ReasoningKernel
newReasoningKernel =
  c_fact_reasoning_kernel_new >>= \(CReasoningKernel ptr) ->
  newForeignPtr c_fact_reasoning_kernel_free ptr

preprocessKb :: ReasoningKernel -> IO ()
preprocessKb k = withForeignPtr k $ \ptr ->
  c_fact_preprocess_kb (CReasoningKernel ptr)

realiseKb :: ReasoningKernel -> IO ()
realiseKb k = withForeignPtr k $ \ptr ->
  c_fact_realise_kb (CReasoningKernel ptr)

classifyKb :: ReasoningKernel -> IO ()
classifyKb k = withForeignPtr k $ \ptr ->
  c_fact_classify_kb (CReasoningKernel ptr)

mkConcept :: ReasoningKernel -> String -> IO ConceptExpression
mkConcept = concept

concept :: ReasoningKernel -> String -> IO ConceptExpression
concept k str = do
  cStr <- newCString str
  withForeignPtr k $ \ptr ->
    c_fact_concept (CReasoningKernel ptr) cStr

mkObjectRole :: ReasoningKernel -> String -> IO ObjectRoleExpression
mkObjectRole = objectRole

objectRole :: ReasoningKernel -> String -> IO ObjectRoleExpression
objectRole k str = do
  cStr <- newCString str
  withForeignPtr k $ \ptr ->
    c_fact_object_role (CReasoningKernel ptr) cStr

relatedTo
  :: ReasoningKernel
  -> IndividualExpression
  -> ObjectRoleExpression
  -> IndividualExpression
  -> IO Axiom
relatedTo k i o j = do
  withForeignPtr k $ \ptr ->
    c_fact_related_to (CReasoningKernel ptr) i o j

valueOf
  :: ReasoningKernel
  -> IndividualExpression
  -> DataRoleExpression
  -> DataValueExpression
  -> IO Axiom
valueOf k i dr dv = do
  withForeignPtr k $ \ptr ->
    c_fact_value_of (CReasoningKernel ptr) i dr dv

instanceOf
  :: ReasoningKernel
  -> IndividualExpression
  -> ConceptExpression
  -> IO Axiom
instanceOf k i c = do
  withForeignPtr k $ \ptr ->
    c_fact_instance_of (CReasoningKernel ptr) i c

newConceptActor :: IO Actor
newConceptActor =
  c_fact_concept_actor_new >>= \(CActor ptr) ->
  newForeignPtr c_fact_actor_free ptr

getSubConcepts :: ReasoningKernel -> ConceptExpression -> RelativesInfo -> IO Actor
getSubConcepts k c rInfo =
  withForeignPtr k $ \ptr -> do
    (CActor actorPtr) <- c_fact_concept_actor_new
    alloca $ \actorPtrPtr -> do
      poke actorPtrPtr actorPtr
      c_fact_get_sub_concepts (CReasoningKernel ptr) c direct actorPtrPtr
      newForeignPtr c_fact_actor_free actorPtr
  where direct = case rInfo of { Direct -> 1 ; All -> 0 }

getInstances :: ReasoningKernel -> ConceptExpression -> IO Actor
getInstances k c =
  withForeignPtr k $ \ptr -> do
    (CActor actorPtr) <- c_fact_concept_actor_new
    alloca $ \actorPtrPtr -> do
      poke actorPtrPtr actorPtr
      c_fact_get_instances (CReasoningKernel ptr) c actorPtrPtr
      newForeignPtr c_fact_actor_free actorPtr

isInstance :: ReasoningKernel -> IndividualExpression -> ConceptExpression -> IO Bool
isInstance k i c = do
  n <- withForeignPtr k $ \ptr -> c_fact_is_instance (CReasoningKernel ptr) i c
  case n of { 0 -> return False; _ -> return True }

getElements1D :: Actor -> IO [String]
getElements1D act =
  withForeignPtr act $ \ptr -> do
    cStrs <- c_fact_get_elements_1d (CActor ptr) >>= peekArray0 nullCString
    mapM peekCString cStrs

getElements2D :: Actor -> IO [[String]]
getElements2D act =
  withForeignPtr act $ \ptr -> do
    cStrs' <- c_fact_get_elements_2d (CActor ptr) >>= peekArray0 nullPtr :: IO [Ptr CString]
    cStrs <- mapM (peekArray0 nullCString) cStrs' :: IO [[CString]]
    mapM (mapM peekCString) cStrs

nullCString :: CString
nullCString = nullPtr
