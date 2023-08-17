grammar Fortran2018;

// R0001 digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
digit:
	'0'
	| '1'
	| '2'
	| '3'
	| '4'
	| '5'
	| '6'
	| '7'
	| '8'
	| '9';

// R0002 letter -> A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U
// | V | W | X | Y | Z
letter:
	'A'
	| 'B'
	| 'C'
	| 'D'
	| 'E'
	| 'F'
	| 'G'
	| 'H'
	| 'I'
	| 'J'
	| 'K'
	| 'L'
	| 'M'
	| 'N'
	| 'O'
	| 'P'
	| 'Q'
	| 'R'
	| 'S'
	| 'T'
	| 'U'
	| 'V'
	| 'W'
	| 'X'
	| 'Y'
	| 'Z';

//R0003 repChar
repChar: NON_CONTROL_CHAR | ESCAPE_SEQUENCE;

NON_CONTROL_CHAR: ~[\u0000-\u001F];

ESCAPE_SEQUENCE: '\\' ('\\' | 'n' | 't' | '"');

// R401 xzy-list -> xzy [, xzy]...
typeAttrSpecList: (typeAttrSpec)+;

typeParamNameList: (typeParamName)+;

typeParamDeclList: (typeParamDecl)+;

componentAttrSpecList: (componentAttrSpec)+;

explicitShapeSpecList: (explicitShapeSpec)+;

deferredShapeSpecList: (deferredShapeSpec)+;

procComponentAttrSpecList: (procComponentAttrSpec)+;

procDeclList: (procDecl)+;

bindAttrList: (bindAttr)+;

typeBoundProcDeclList: (typeBoundProcDecl)+;

bindingNameList: (bindingName)+;

finalSubroutineNameList: (finalSubroutineName)+;

typeParamSpecList: (typeParamSpec)+;

componentSpecList: (componentSpec)+;

enumeratorList: (enumerator)+;

acValueList: (acValue)+;

entityDeclList: (entityDecl)+;

deferredCoShapeSpecList: (deferredCoShapeSpec)+;

assumedShapeSpecList: (assumedShapeSpec)+;

assumedImpliedSpecList: (assumedImpliedSpec)+;

accessIdList: (accessId)+;

allocatableDeclList: (allocatableDecl)+;

objectNameList: (objectName)+;

bindEntityList: (bindEntity)+;

codimensionDeclList: (codimensionDecl)+;

dataStmtObjectList: (dataStmtObject)+;

dataStmtValueList: (dataStmtValue)+;

dataIDoObjectList: (dataIDoObject)+;

dummyArgNameList: (dummyArgName)+;

namedConstantDefList: (namedConstantDef)+;

pointerDeclList: (pointerDecl)+;

entityNameList: (entityName)+;

savedEntityList: (savedEntity)+;

targetDeclList: (targetDecl)+;

implicitSpecList: (implicitSpec)+;

implicitNameSpecList: (implicitNameSpec)+;

letterSpecList: (letterSpec)+;

importNameList: (importName)+;

namelistGroupObjectList: (namelistGroupObject)+;

equivalenceSetList: (equivalenceSet)+;

equivalenceObjectList: (equivalenceObject)+;

commonBlockObjectList: (commonBlockObject)+;

sectionSubscriptList: (sectionSubscript)+;

cosubscriptList: (cosubscript)+;

imageSelectorSpecList: (imageSelectorSpec)+;

allocationList: (allocation)+;

allocOptList: (allocOpt)+;

allocateShapeSpecList: (allocateShapeSpec)+;

allocateCoshapeSpecList: (allocateCoshapeSpec)+;

pointerObjectList: (pointerObject)+;

allocateObjectList: (allocateObject)+;

deallocOptList: (deallocOpt)+;

boundsSpecList: (boundsSpec)+;

boundsRemappingList: (boundsRemapping)+;

associationList: (association)+;

coarrayAssociationList: (coarrayAssociation)+;

syncStatList: (syncStat)+;

concurrentControlList: (concurrentControl)+;

variableNameList: (variableName)+;

caseValueRangeList: (caseValueRange)+;

labelList: (label)+;

eventWaitSpecList: (eventWaitSpec)+;

formTeamSpecList: (formTeamSpec)+;

lockStatList: (lockStat)+;

connectSpecList: (connectSpec)+;

closeSpecList: (closeSpec)+;

ioControlSpecList: (ioControlSpec)+;

inputItemList: (inputItem)+;

outputItemList: (outputItem)+;

ioImpliedDoObjectList: (ioImpliedDoObject)+;

waitSpecList: (waitSpec)+;

positionSpecList: (positionSpec)+;

flushSpecList: (flushSpec)+;

inquireSpecList: (inquireSpec)+;

vList: (v)+;

renameList: (rename)+;

onlyList: (only)+;

specificProcedureList: (specificProcedure)+;

externalNameList: (externalName)+;

intrinsicProcedureNameList: (intrinsicProcedureName)+;

actualArgSpecList: (actualArgSpec)+;

dummyArgList: (dummyArg)+;

// R402 xzy-name -> name
typeName: name;

typeParamName: name;

parentTypeName: name;

componentName: name;

argName: name;

bindingName: name;

procedureName: name;

finalSubroutineName: name;

functionName: name;

accessName: name;

entityName: name;

commonBlockName: name;

coarrayName: name;

arrayName: name;

procEntityName: name;

namelistGroupName: name;

importName: name;

partName: name;

dataPointerComponentName: name;

associateName: name;

procedureComponentName: name;

whereConstructName: name;

forallConstructName: name;

associateConstructName: name;

blockConstructName: name;

teamConstructName: name;

criticalConstructName: name;

doConstructName: name;

indexName: name;

ifConstructName: name;

caseConstructName: name;

selectConstructName: name;

constructName: name;

moduleName: name;

programName: name;

localName: name;

blockDataName: name;

genericName: name;

useName: name;

submoduleName: name;

ancestorModuleName: name;

parentSubmoduleName: name;

procedureEntityName: name;

externalName: name;

intrinsicProcedureName: name;

resultName: name;

subroutineName: name;

entryName: name;

intConstantName: name;

intVariableName: name;

// R403 scalar-xyz -> xyz
scalarIntExpr: intExpr;

scalarIntConstantExpr: intConstantExpr;

scalarDefaultCharConstantExpr: defaultCharConstantExpr;

scalarStructureComponent: structureComponent;

scalarIntConstant: intConstant;

scalarIntConstantSubobject: intConstantSubobject;

scalarConstant: constant;

scalarConstantSubobject: constantSubobject;

scalarCharLiteralConstant: charLiteralConstant;

scalarNamedConstant: namedConstant;

scalarIntVariable: intVariable;

scalarDefaultCharVariable: defaultCharVariable;

scalarVariable: variable;

scalarExpr: expr;

scalarLogicalExpr: logicalExpr;

scalarMaskExpr: maskExpr;

scalarConstantExpr: constantExpr;

scalarDefaultCharExpr: defaultCharExpr;

scalarLogicalVariable: logicalVariable;

scalarIntConstantName: intConstantName;

scalarVariableName: variableName;

scalarIntVariableName: intVariableName;

//R501 program -> program-unit [program-unit]...    
program: programUnit (programUnit)*;

//R502 program-unit -> main-program | external-subprogram | module | submodule | block-data
programUnit:
	mainProgram
	| externalSubprogram
	| module
	| submodule
	| blockData;

//R503 external-subprogram -> function-subprogram | subroutine-subprogram
externalSubprogram: functionSubprogram | subroutineSubprogram;

//R504 specification-part -> [use-stmt]... [import-stmt]... [implicit-part]
// [declaration-construct]...
specificationPart:
	(useStmt)* (importStmt)* (implicitPart)? (
		declarationConstruct
	)*;

//R505 implicit-part -> [implicit-part-stmt]... implicit-stmt
implicitPart: (implicitPartStmt)* implicitStmt;

//R506 implicit-part-stmt -> implicit-stmt | parameter-stmt | format-stmt | entry-stmt
implicitPartStmt:
	implicitStmt
	| parameterStmt
	| formatStmt
	| entryStmt;

// R507 declaration-construct -> specification-construct | data-stmt | format-stmt | entry-stmt |
// stmt-function-stmt
declarationConstruct:
	specificationConstruct
	| dataStmt
	| formatStmt
	| entryStmt
	| stmtFunctionStmt;

// R508 specification-construct -> derived-type-def | enum-def | generic-stmt | interface-block |
// parameter-stmt | procedure-declaration-stmt | other-specification-stmt | type-declaration-stmt
specificationConstruct:
	derivedTypeDef
	| enumDef
	| genericStmt
	| interfaceBlock
	| parameterStmt
	| procedureDeclarationStmt
	| otherSpecificationStmt
	| typeDeclarationStmt;

// R509 execution-part -> executable-construct [execution-part-construct]...
executionPart: executableConstruct (executionPartConstruct)*;

// R510 execution-part-construct -> executable-construct | format-stmt | entry-stmt | data-stmt
executionPartConstruct:
	executableConstruct
	| formatStmt
	| entryStmt
	| dataStmt;

// R511 internal-subprogram-part -> contains-stmt [internal-subprogram]...
internalSubprogramPart: containsStmt (internalSubprogram)*;

// R512 internal-subprogram -> function-subprogram | subroutine-subprogram
internalSubprogram: functionSubprogram | subroutineSubprogram;

// R513 other-specification-stmt -> access-stmt | allocatable-stmt | asynchronous-stmt | bind-stmt |
// codimension-stmt | contiguous-stmt | dimension-stmt | external-stmt | intent-stmt |
// intrinsic-stmt | namelist-stmt | optional-stmt | pointer-stmt | protected-stmt | save-stmt |
// target-stmt | volatile-stmt | value-stmt | common-stmt | equivalence-stmt
otherSpecificationStmt:
	accessStmt
	| allocatableStmt
	| asynchronousStmt
	| bindStmt
	| codimensionStmt
	| contiguousStmt
	| dimensionStmt
	| externalStmt
	| intentStmt
	| intrinsicStmt
	| namelistStmt
	| optionalStmt
	| pointerStmt
	| protectedStmt
	| saveStmt
	| targetStmt
	| volatileStmt
	| valueStmt
	| commonStmt
	| equivalenceStmt;

// R514 executable-construct -> action-stmt | associate-construct | block-construct | case-construct
// | change-team-construct | critical-construct | do-construct | if-construct |
// select-rank-construct | select-type-construct | where-construct | forall-construct
executableConstruct:
	actionStmt
	| associateConstruct
	| blockConstruct
	| caseConstruct
	| changeTeamConstruct
	| criticalConstruct
	| doConstruct
	| ifConstruct
	| selectRankConstruct
	| selectTypeConstruct
	| whereConstruct
	| forallConstruct;

// R515 action-stmt -> allocate-stmt | assignment-stmt | backspace-stmt | call-stmt | close-stmt |
// continue-stmt | cycle-stmt | deallocate-stmt | endfile-stmt | error-stop-stmt | event-post-stmt |
// event-wait-stmt | exit-stmt | fail-image-stmt | flush-stmt | form-team-stmt | goto-stmt | if-stmt
// | inquire-stmt | lock-stmt | nullify-stmt | open-stmt | pointer-assignment-stmt | print-stmt |
// read-stmt | return-stmt | rewind-stmt | stop-stmt | sync-all-stmt | sync-images-stmt |
// sync-memory-stmt | sync-team-stmt | unlock-stmt | wait-stmt | where-stmt | write-stmt |
// computed-goto-stmt | forall-stmt
actionStmt:
	allocateStmt
	| assignmentStmt
	| backspaceStmt
	| callStmt
	| closeStmt
	| continueStmt
	| cycleStmt
	| deallocateStmt
	| endfileStmt
	| errorStopStmt
	| eventPostStmt
	| eventWaitStmt
	| exitStmt
	| failImageStmt
	| flushStmt
	| formTeamStmt
	| gotoStmt
	| ifStmt
	| inquireStmt
	| lockStmt
	| nullifyStmt
	| openStmt
	| pointerAssignmentStmt
	| printStmt
	| readStmt
	| returnStmt
	| rewindStmt
	| stopStmt
	| syncAllStmt
	| syncImagesStmt
	| syncMemoryStmt
	| syncTeamStmt
	| unlockStmt
	| waitStmt
	| whereStmt
	| writeStmt
	| computedGotoStmt
	| forallStmt;

// R516 keyword -> name
keyword: name;

// R601 alphanumeric-character -> letter | digit | UNDERSCORE 
alphanumericCharacter: letter | digit | UNDERSCORE;

// R602 UNDERSCORE -> _
UNDERSCORE: '_';

// R603 name -> letter [alphanumeric-character]...
name: letter (alphanumericCharacter)*;

// R604 constant -> literal-constant | named-constant
constant: literalConstant | namedConstant;

// R605 literal-constant -> int-literal-constant | real-literal-constant | complex-literal-constant
// | logical-literal-constant | char-literal-constant | boz-literal-constant
literalConstant:
	intLiteralConstant
	| realLiteralConstant
	| complexLiteralConstant
	| logicalLiteralConstant
	| charLiteralConstant
	| bozLiteralConstant;

// R606 named-constant -> name
namedConstant: name;

// R607 int-constant -> constant
intConstant: constant;

// R608 intrinsic-operator -> power-op | mult-op | add-op | concat-op | rel-op | not-op | and-op |
// or-op | equiv-op
intrinsicOperator:
	powerOp
	| multOp
	| addOp
	| concatOp
	| relOp
	| notOp
	| andOp
	| orOp
	| equivOp;

// R609 defined-operator -> defined-unary-op | defined-binary-op | extended-intrinsic-op
definedOperator:
	definedUnaryOp
	| definedBinaryOp
	| extendedIntrinsicOp;

// R610 extended-intrinsic-op -> intrinsic-operator
extendedIntrinsicOp: intrinsicOperator;

// R611 label -> digit [digit]...
label: digit (digit)*;

// R620 delimiter -> ( | ) | / | [ | ] | (/ | /)
delimiter: '(' | ')' | '/' | '[' | ']' | '(/' | '/)';

// R701 type-param-value -> scalar-int-expr | * | :
typeParamValue: scalarIntExpr | '*' | ':';

// R702 type-spec -> intrinsic-type-spec | derived-type-spec
typeSpec: intrinsicTypeSpec | derivedTypeSpec;

// R703 declaration-type-spec -> intrinsic-type-spec | TYPE ( intrinsic-type-spec ) | TYPE (
// derived-type-spec ) | CLASS ( derived-type-spec ) | CLASS ( * ) | TYPE ( * )
declarationTypeSpec:
	intrinsicTypeSpec
	| 'TYPE' '(' intrinsicTypeSpec ')'
	| 'TYPE' '(' derivedTypeSpec ')'
	| 'CLASS' '(' derivedTypeSpec ')'
	| 'CLASS' '(' '*' ')'
	| 'TYPE' '(' '*' ')';

// R704 intrinsic-type-spec -> integer-type-spec | REAL [kind-selector] | DOUBLE PRECISION | COMPLEX
// [kind-selector] | CHARACTER [char-selector] | LOGICAL [kind-selector]
intrinsicTypeSpec:
	integerTypeSpec
	| 'REAL' kindSelector?
	| 'DOUBLE' 'PRECISION'
	| 'COMPLEX' kindSelector?
	| 'CHARACTER' charSelector?
	| 'LOGICAL' kindSelector?;

// R705 integer-type-spec -> INTEGER [kind-selector]
integerTypeSpec: 'INTEGER' kindSelector?;

// R706 kind-selector -> ( [KIND =] scalar-int-constant-expr )
kindSelector: '(' ('KIND' '=')? scalarIntConstantExpr ')';

// R707 signed-int-literal-constant -> [sign] int-literal-constant
signedIntLiteralConstant: sign? intLiteralConstant;

// R708 int-literal-constant -> digit-string [_ kind-param]
intLiteralConstant: digitString ('_' kindParam)?;

// R709 kind-param -> digit-string | scalar-int-constant-name
kindParam: digitString | scalarIntConstantName;

// R710 signed-digit-string -> [sign] digit-string
signedDigitString: sign? digitString;

// R711 digit-string -> digit [digit]...
digitString: digit (digit)*;

// R712 sign -> + | -
sign: '+' | '-';

// R713 signed-real-literal-constant -> [sign] real-literal-constant
signedRealLiteralConstant: sign? realLiteralConstant;

// R714 real-literal-constant -> significand [exponent-letter exponent] [_ kind-param] |
// digit-string exponent-letter exponent [_ kind-param]
realLiteralConstant:
	significand (exponentLetter exponent)? ('_' kindParam)?
	| digitString exponentLetter exponent ('_' kindParam)?;

// R715 significand -> digit-string . [digit-string] | . digit-string
significand: digitString '.' (digitString)? | '.' digitString;

// R716 exponent-letter -> E | D
exponentLetter: 'E' | 'D';

// R717 exponent -> signed-digit-string
exponent: signedDigitString;

// R718 complex-literal-constant -> ( real-part , imag-part )
complexLiteralConstant: '(' realPart ',' imagPart ')';

// R719 real-part -> signed-int-literal-constant | signed-real-literal-constant | named-constant
realPart:
	signedIntLiteralConstant
	| signedRealLiteralConstant
	| namedConstant;

// R720 imag-part -> signed-int-literal-constant | signed-real-literal-constant | named-constant
imagPart:
	signedIntLiteralConstant
	| signedRealLiteralConstant
	| namedConstant;

// R721 char-selector -> length-selector | ( LEN = type-param-value , KIND =
// scalar-int-constant-expr ) | ( type-param-value , [KIND =] scalar-int-constant-expr ) | ( KIND =
// scalar-int-constant-expr [, LEN = type-param-value] )
charSelector:
	lengthSelector
	| '(' 'LEN' '=' typeParamValue ',' 'KIND' '=' scalarIntConstantExpr ')'
	| '(' typeParamValue ',' ('KIND' '=')? scalarIntConstantExpr ')'
	| '(' 'KIND' '=' scalarIntConstantExpr (
		',' 'LEN' '=' typeParamValue
	)? ')';

// R722 length-selector -> ( [LEN =] type-param-value ) | * char-length [,]
lengthSelector:
	'(' ('LEN' '=')? typeParamValue ')'
	| '*' charLength (',')?;

// R723 char-length -> ( type-param-value ) | digit-string
charLength: '(' typeParamValue ')' | digitString;

// R724 char-literal-constant -> [kind-param _] ' [rep-char]... ' | [kind-param _] " [rep-char]... "
charLiteralConstant: (kindParam '_')? '\'' (repChar)* '\''
	| (kindParam '_')? '"' (repChar)* '"';

// R725 logical-literal-constant -> .TRUE. [_ kind-param] | .FALSE. [_ kind-param]
logicalLiteralConstant:
	'.TRUE.' ('_' kindParam)?
	| '.FALSE.' ('_' kindParam)?;

// R726 derived-type-def -> derived-type-stmt [type-param-def-stmt]... [private-or-sequence]...
// [component-part] [type-bound-procedure-part] end-type-stmt
derivedTypeDef:
	derivedTypeStmt (typeParamDefStmt)* (privateOrSequence)* componentPart? typeBoundProcedurePart?
		endTypeStmt;

// R727 derived-type-stmt -> TYPE [[, type-attr-spec-list] ::] type-name [( type-param-name-list )]
derivedTypeStmt:
	'TYPE' ((',' typeAttrSpecList)? '::')? typeName (
		'(' typeParamNameList ')'
	)?;

// R728 type-attr-spec -> ABSTRACT | access-spec | BIND(C) | EXTENDS ( parent-type-name )
typeAttrSpec:
	'ABSTRACT'
	| accessSpec
	| 'BIND(C)'
	| 'EXTENDS' '(' parentTypeName ')';

// R729 private-or-sequence -> private-components-stmt | sequence-stmt
privateOrSequence: privateComponentsStmt | sequenceStmt;

// R730 end-type-stmt -> END TYPE [type-name]
endTypeStmt: 'END' 'TYPE' typeName?;

// R731 sequence-stmt -> SEQUENCE
sequenceStmt: 'SEQUENCE';

// R732 type-param-def-stmt -> integer-type-spec , type-param-attr-spec :: type-param-decl-list
typeParamDefStmt:
	integerTypeSpec ',' typeParamAttrSpec '::' typeParamDeclList;

// R733 type-param-decl -> type-param-name [= scalar-int-constant-expr]
typeParamDecl: typeParamName ('=' scalarIntConstantExpr)?;

// R734 type-param-attr-spec -> KIND | LEN
typeParamAttrSpec: 'KIND' | 'LEN';

// R735 component-part -> [component-def-stmt]...
componentPart: (componentDefStmt)*;

// R736 component-def-stmt -> data-component-def-stmt | proc-component-def-stmt
componentDefStmt: dataComponentDefStmt | procComponentDefStmt;

// R737 data-component-def-stmt -> declaration-type-spec [[, component-attr-spec-list] ::]
// component-decl-list
dataComponentDefStmt:
	declarationTypeSpec ((',' componentAttrSpecList)? '::')?;

// R738 component-attr-spec -> access-spec | ALLOCATABLE | CODIMENSION lbracket coarray-spec
// rbracket | CONTIGUOUS | DIMENSION ( component-array-spec ) | POINTER
componentAttrSpec:
	accessSpec
	| 'ALLOCATABLE'
	| 'CODIMENSION' lbracket coarraySpec rbracket
	| 'CONTIGUOUS'
	| 'DIMENSION' '(' componentArraySpec ')'
	| 'POINTER';

// R739 component-decl -> component-name [( component-array-spec )] [lbracket coarray-spec rbracket]
// [* char-length] [component-initialization]
componentDecl:
	componentName ('(' componentArraySpec ')')? (
		lbracket coarraySpec rbracket
	)? ('*' charLength)? componentInitialization?;

// R740 component-array-spec -> explicit-shape-spec-list | deferred-shape-spec-list
componentArraySpec:
	explicitShapeSpecList
	| deferredShapeSpecList;

// R741 proc-component-def-stmt -> PROCEDURE ( [proc-interface] ) , proc-component-attr-spec-list ::
// proc-decl-list
procComponentDefStmt:
	'PROCEDURE' '(' procInterface? ')' ',' procComponentAttrSpecList '::' procDeclList;

// R742 proc-component-attr-spec -> access-spec | NOPASS | PASS [(arg-name)] | POINTER
procComponentAttrSpec:
	accessSpec
	| 'NOPASS'
	| 'PASS' ('(' argName ')')?
	| 'POINTER';

// R743 component-initialization -> = constant-expr | => null-init | => initial-data-target
componentInitialization:
	'=' constantExpr
	| '=>' nullInit
	| '=>' initialDataTarget;

// R744 initial-data-target -> designator
initialDataTarget: designator;

// R745 private-components-stmt -> PRIVATE
privateComponentsStmt: 'PRIVATE';

// R746 type-bound-procedure-part -> contains-stmt [binding-private-stmt]
// [type-bound-proc-binding]...
typeBoundProcedurePart:
	containsStmt (bindingPrivateStmt)? (typeBoundProcBinding)*;

// R747 binding-private-stmt -> PRIVATE
bindingPrivateStmt: 'PRIVATE';

// R748 type-bound-proc-binding -> type-bound-procedure-stmt | type-bound-generic-stmt |
// final-procedure-stmt
typeBoundProcBinding:
	typeBoundProcedureStmt
	| typeBoundGenericStmt
	| finalProcedureStmt;

// R749 type-bound-procedure-stmt -> PROCEDURE [[, bind-attr-list] ::] type-bound-proc-decl-list |
// PROCEDURE ( interface-name ) , bind-attr-list :: binding-name-list
typeBoundProcedureStmt:
	'PROCEDURE' ((',' bindAttrList)? '::')? typeBoundProcDeclList
	| 'PROCEDURE' '(' interfaceName ')' ',' bindAttrList '::' bindingNameList;

// R750 type-bound-proc-decl -> binding-name [=> procedure-name]
typeBoundProcDecl: bindingName ('=>' procedureName)?;

// R751 type-bound-generic-stmt -> GENERIC [, access-spec] :: generic-spec => binding-name-list
typeBoundGenericStmt:
	'GENERIC' (',' accessSpec)? '::' genericSpec '=>' bindingNameList;

// R752 bind-attr -> access-spec | DEFERRED | NON_OVERRIDABLE | NOPASS | PASS [(arg-name)]
bindAttr:
	accessSpec
	| 'DEFERRED'
	| 'NON_OVERRIDABLE'
	| 'NOPASS'
	| 'PASS' ('(' argName ')')?;

// R753 final-procedure-stmt -> FINAL [::] final-subroutine-name-list
finalProcedureStmt: 'FINAL' ('::')? finalSubroutineNameList;

// R754 derived-type-spec -> type-name [(type-param-spec-list)]
derivedTypeSpec: typeName ('(' typeParamSpecList ')')?;

// R755 type-param-spec -> [keyword =] type-param-value
typeParamSpec: (keyword '=')? typeParamValue;

// R756 structure-constructor -> derived-type-spec ( [component-spec-list] )
structureConstructor:
	derivedTypeSpec '(' (componentSpecList)? ')';

// R757 component-spec -> [keyword =] component-data-source
componentSpec: (keyword '=')? componentDataSource;

// R758 component-data-source -> expr | data-target | proc-target
componentDataSource: expr | dataTarget | procTarget;

// R759 enum-def -> enum-def-stmt enumerator-def-stmt [enumerator-def-stmt]... end-enum-stmt
enumDef: enumDefStmt enumeratorDefStmt+ endEnumStmt;

// R760 enum-def-stmt -> ENUM, BIND(C)
enumDefStmt: 'ENUM' ',' 'BIND(C)';

// R761 enumerator-def-stmt -> ENUMERATOR [::] enumerator-list
enumeratorDefStmt: 'ENUMERATOR' ('::')? enumeratorList;

// R762 enumerator -> named-constant [= scalar-int-constant-expr]
enumerator: namedConstant ('=' scalarIntConstantExpr)?;

// R763 end-enum-stmt -> END ENUM
endEnumStmt: 'END' 'ENUM';

// R764 boz-literal-constant -> binary-constant | octal-constant | hex-constant
bozLiteralConstant:
	binaryConstant
	| octalConstant
	| hexConstant;

// R765 binary-constant -> B ' digit [digit]... ' | B " digit [digit]... "
binaryConstant: 'B' '\'' digit+ '\'' | 'B' '"' digit+ '"';

// R766 octal-constant -> O ' digit [digit]... ' | O " digit [digit]... "
octalConstant: 'O' '\'' digit+ '\'' | 'O' '"' digit+ '"';

// R767 hex-constant -> Z ' hex-digit [hex-digit]... ' | Z " hex-digit [hex-digit]... "
hexConstant: 'Z' '\'' hexDigit+ '\'' | 'Z' '"' hexDigit+ '"';

// R768 hex-digit -> digit | A | B | C | D | E | F
hexDigit: digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F';

// R769 array-constructor -> (/ ac-spec /) | lbracket ac-spec rbracket
arrayConstructor: '(/' acSpec '/)' | lbracket acSpec rbracket;

// R770 ac-spec -> type-spec :: | [type-spec ::] ac-value-list
acSpec: typeSpec '::' | (typeSpec '::')? acValueList;

// R771 lbracket -> [
lbracket: '[';

// R772 rbracket -> ]
rbracket: ']';

// R773 ac-value -> expr | ac-implied-do
acValue: expr | acImpliedDo;

// R774 ac-implied-do -> ( ac-value-list , ac-implied-do-control )
acImpliedDo: '(' acValueList ',' acImpliedDoControl ')';

// R775 ac-implied-do-control -> [integer-type-spec ::] ac-do-variable = scalar-int-expr ,
// scalar-int-expr [, scalar-int-expr]
acImpliedDoControl:
	(integerTypeSpec '::')? acDoVariable '=' scalarIntExpr ',' scalarIntExpr (
		',' scalarIntExpr
	)?;

// R776 ac-do-variable -> do-variable
acDoVariable: doVariable;

// R801 type-declaration-stmt -> declaration-type-spec [[, attr-spec]... ::] entity-decl-list
typeDeclarationStmt:
	declarationTypeSpec ((',' attrSpec)* '::')? entityDeclList;

// R802 attr-spec -> access-spec | ALLOCATABLE | ASYNCHRONOUS | CODIMENSION lbracket coarray-spec
// rbracket | CONTIGUOUS | DIMENSION ( array-spec ) | EXTERNAL | INTENT ( intent-spec ) | INTRINSIC
// | language-binding-spec | OPTIONAL | PARAMETER | POINTER | PROTECTED | SAVE | TARGET | VALUE |
// VOLATILE
attrSpec:
	accessSpec
	| 'ALLOCATABLE'
	| 'ASYNCHRONOUS'
	| 'CODIMENSION' lbracket coarraySpec rbracket
	| 'CONTIGUOUS'
	| 'DIMENSION' '(' arraySpec ')'
	| 'EXTERNAL'
	| 'INTENT' '(' intentSpec ')'
	| 'INTRINSIC'
	| languageBindingSpec
	| 'OPTIONAL'
	| 'PARAMETER'
	| 'POINTER'
	| 'PROTECTED'
	| 'SAVE'
	| 'TARGET'
	| 'VALUE'
	| 'VOLATILE';

// R803 entity-decl -> object-name [( array-spec )] [lbracket coarray-spec rbracket] [* char-length]
// [initialization] | function-name [* char-length]
entityDecl:
	objectName ('(' arraySpec ')')? (
		lbracket coarraySpec rbracket
	)? ('*' charLength)? (initialization)?
	| functionName ('*' charLength)?;

// R804 object-name -> name
objectName: name;

// R805 initialization -> = constant-expr | => null-init | => initial-data-target
initialization:
	'=' constantExpr
	| '=>' nullInit
	| '=>' initialDataTarget;

// R806 null-init -> function-reference
nullInit: functionReference;

// R807 access-spec -> PUBLIC | PRIVATE
accessSpec: 'PUBLIC' | 'PRIVATE';

// R808 language-binding-spec -> BIND ( C [, NAME = scalar-default-char-constant-expr] )
languageBindingSpec:
	'BIND' '(' 'C' (',' 'NAME' '=' scalarDefaultCharConstantExpr)? ')';

// R809 coarray-spec -> deferred-coshape-spec-list | explicit-coshape-spec
coarraySpec: deferredCoShapeSpecList | explicitCoShapeSpec;

// R810 deferred-coshape-spec -> :
deferredCoShapeSpec: ':';

// R811 explicit-coshape-spec -> [[lower-cobound :] upper-cobound ,]... [lower-cobound :] *
explicitCoShapeSpec: ((lowerCoBound ':')? upperCoBound ',')* (
		lowerCoBound? ':'
	)? '*';

// R812 lower-cobound -> specification-expr
lowerCoBound: specificationExpr;

// R813 upper-cobound -> specification-expr
upperCoBound: specificationExpr;

// R814 dimension-spec -> DIMENSION ( array-spec )
dimensionSpec: 'DIMENSION' '(' arraySpec ')';

// R815 array-spec -> explicit-shape-spec-list | assumed-shape-spec-list | deferred-shape-spec-list
// | assumed-size-spec | implied-shape-spec | implied-shape-or-assumed-size-spec | assumed-rank-spec
arraySpec:
	explicitShapeSpecList
	| assumedShapeSpecList
	| deferredShapeSpecList
	| assumedSizeSpec
	| impliedShapeSpec
	| impliedShapeOrAssumedSizeSpec
	| assumedRankSpec;

// R816 explicit-shape-spec -> [lower-bound :] upper-bound
explicitShapeSpec: (lowerBound ':')? upperBound;

// R817 lower-bound -> specification-expr
lowerBound: specificationExpr;

// R818 upper-bound -> specification-expr
upperBound: specificationExpr;

// R819 assumed-shape-spec -> [lower-bound] :
assumedShapeSpec: (lowerBound)? ':';

// R820 deferred-shape-spec -> :
deferredShapeSpec: ':';

// R821 assumed-implied-spec -> [lower-bound :] *
assumedImpliedSpec: (lowerBound ':')? '*';

// R822 assumed-size-spec -> explicit-shape-spec-list , assumed-implied-spec
assumedSizeSpec: explicitShapeSpecList ',' assumedImpliedSpec;

// R823 implied-shape-or-assumed-size-spec -> assumed-implied-spec
impliedShapeOrAssumedSizeSpec: assumedImpliedSpec;

// R824 implied-shape-spec -> assumed-implied-spec , assumed-implied-spec-list
impliedShapeSpec: assumedImpliedSpec ',' assumedImpliedSpecList;

// R825 assumed-rank-spec -> ..
assumedRankSpec: '..';

// R826 intent-spec -> IN | OUT | INOUT
intentSpec: 'IN' | 'OUT' | 'INOUT';

// R827 access-stmt -> access-spec [[::] access-id-list]
accessStmt: accessSpec ('::'? accessIdList)?;

// R828 access-id -> access-name | generic-spec
accessId: accessName | genericSpec;

// R829 allocatable-stmt -> ALLOCATABLE [::] allocatable-decl-list
allocatableStmt: 'ALLOCATABLE' '::'? allocatableDeclList;

// R830 allocatable-decl -> object-name [( array-spec )] [lbracket coarray-spec rbracket]
allocatableDecl:
	objectName ('(' arraySpec ')')? (
		lbracket coarraySpec rbracket
	)?;

// R831 asynchronous-stmt -> ASYNCHRONOUS [::] object-name-list
asynchronousStmt: 'ASYNCHRONOUS' '::'? objectNameList;

// R832 bind-stmt -> language-binding-spec [::] bind-entity-list
bindStmt: languageBindingSpec '::'? bindEntityList;

// R833 bind-entity -> entity-name | / common-block-name /
bindEntity: entityName | '/' commonBlockName '/';

// R834 codimension-stmt -> CODIMENSION [::] codimension-decl-list
codimensionStmt: 'CODIMENSION' '::'? codimensionDeclList;

// R835 codimension-decl -> coarray-name lbracket coarray-spec rbracket
codimensionDecl: coarrayName lbracket coarraySpec rbracket;

// R836 contiguous-stmt -> CONTIGUOUS [::] object-name-list
contiguousStmt: 'CONTIGUOUS' '::'? objectNameList;

// R837 data-stmt -> DATA data-stmt-set [[,] data-stmt-set]...
dataStmt: 'DATA' dataStmtSet (','? dataStmtSet)*;

// R838 data-stmt-set -> data-stmt-object-list / data-stmt-value-list /
dataStmtSet: dataStmtObjectList '/' dataStmtValueList '/';

// R839 data-stmt-object -> variable | data-implied-do
dataStmtObject: variable | dataImpliedDo;

// R840 data-implied-do -> ( data-i-do-object-list , [integer-type-spec ::] data-i-do-variable =
// scalar-int-constant-expr , scalar-int-constant-expr [, scalar-int-constant-expr] )
dataImpliedDo:
	'(' dataIDoObjectList ',' (integerTypeSpec '::')? dataIDoVariable '=' scalarIntConstantExpr ','
		scalarIntConstantExpr (',' scalarIntConstantExpr)? ')';

// R841 data-i-do-object -> array-element | scalar-structure-component | data-implied-do
dataIDoObject:
	arrayElement
	| scalarStructureComponent
	| dataImpliedDo;

// R842 data-i-do-variable -> do-variable
dataIDoVariable: doVariable;

// R843 data-stmt-value -> [data-stmt-repeat *] data-stmt-constant
dataStmtValue: (dataStmtRepeat '*')? dataStmtConstant;

// R844 data-stmt-repeat -> scalar-int-constant | scalar-int-constant-subobject
dataStmtRepeat: scalarIntConstant | scalarIntConstantSubobject;

// R845 data-stmt-constant -> scalar-constant | scalar-constant-subobject |
// signed-int-literal-constant | signed-real-literal-constant | null-init | initial-data-target |
// structure-constructor
dataStmtConstant:
	scalarConstant
	| scalarConstantSubobject
	| signedIntLiteralConstant
	| signedRealLiteralConstant
	| nullInit
	| initialDataTarget
	| structureConstructor;

// R846 int-constant-subobject -> constant-subobject
intConstantSubobject: constantSubobject;

// R847 constant-subobject -> designator
constantSubobject: designator;

// R848 dimension-stmt -> DIMENSION [::] array-name ( array-spec ) [, array-name ( array-spec )]...
dimensionStmt:
	'DIMENSION' '::'? arrayName '(' arraySpec ')' (
		',' arrayName '(' arraySpec ')'
	)*;

// R849 intent-stmt -> INTENT ( intent-spec ) [::] dummy-arg-name-list
intentStmt: 'INTENT' '(' intentSpec ')' '::'? dummyArgNameList;

// R850 optional-stmt -> OPTIONAL [::] dummy-arg-name-list
optionalStmt: 'OPTIONAL' '::'? dummyArgNameList;

// R851 parameter-stmt -> PARAMETER ( named-constant-def-list )
parameterStmt: 'PARAMETER' '(' namedConstantDefList ')';

// R852 named-constant-def -> named-constant = constant-expr
namedConstantDef: namedConstant '=' constantExpr;

// R853 pointer-stmt -> POINTER [::] pointer-decl-list
pointerStmt: 'POINTER' '::'? pointerDeclList;

// R854 pointer-decl -> object-name [( deferred-shape-spec-list )] | proc-entity-name
pointerDecl:
	objectName ('(' deferredShapeSpecList ')')?
	| procEntityName;

// R855 protected-stmt -> PROTECTED [::] entity-name-list
protectedStmt: 'PROTECTED' '::'? entityNameList;

// R856 save-stmt -> SAVE [[::] saved-entity-list]
saveStmt: 'SAVE' ('::'? savedEntityList)?;

// R857 saved-entity -> object-name | proc-pointer-name | / common-block-name /
savedEntity:
	objectName
	| procPointerName
	| '/' commonBlockName '/';

// R858 proc-pointer-name -> name
procPointerName: name;

// R859 target-stmt -> TARGET [::] target-decl-list
targetStmt: 'TARGET' '::'? targetDeclList;

// R860 target-decl -> object-name [( array-spec )] [lbracket coarray-spec rbracket]
targetDecl:
	objectName ('(' arraySpec ')')? (
		lbracket coarraySpec rbracket
	)?;

// R861 value-stmt -> VALUE [::] dummy-arg-name-list
valueStmt: 'VALUE' '::'? dummyArgNameList;

// R862 volatile-stmt -> VOLATILE [::] object-name-list
volatileStmt: 'VOLATILE' '::'? objectNameList;

// R863 implicit-stmt -> IMPLICIT implicit-spec-list | IMPLICIT NONE [( [implicit-name-spec-list] )]
implicitStmt:
	'IMPLICIT' implicitSpecList
	| 'IMPLICIT' 'NONE' ('(' implicitNameSpecList? ')')?;

// R864 implicit-spec -> declaration-type-spec ( letter-spec-list )
implicitSpec: declarationTypeSpec '(' letterSpecList ')';

// R865 letter-spec -> letter [- letter]
letterSpec: letter ('-' letter)?;

// R866 implicit-name-spec -> EXTERNAL | TYPE
implicitNameSpec: 'EXTERNAL' | 'TYPE';

// R867 import-stmt -> IMPORT [[::] import-name-list] | IMPORT , ONLY : import-name-list | IMPORT ,
// NONE | IMPORT , ALL
importStmt:
	'IMPORT' ('::'? importNameList)?
	| 'IMPORT' ',' 'ONLY' ':' importNameList
	| 'IMPORT' ',' 'NONE'
	| 'IMPORT' ',' 'ALL';

// R868 namelist-stmt -> NAMELIST / namelist-group-name / namelist-group-object-list [[,] /
// namelist-group-name / namelist-group-object-list]...
namelistStmt:
	'NAMELIST' '/' namelistGroupName '/' namelistGroupObjectList (
		','? '/' namelistGroupName '/' namelistGroupObjectList
	)*;

// R869 namelist-group-object -> variable-name
namelistGroupObject: variableName;

// R870 equivalence-stmt -> EQUIVALENCE equivalence-set-list
equivalenceStmt: 'EQUIVALENCE' equivalenceSetList;

// R871 equivalence-set -> ( equivalence-object , equivalence-object-list )
equivalenceSet:
	'(' equivalenceObject ',' equivalenceObjectList ')';

// R872 equivalence-object -> variable-name | array-element | substring
equivalenceObject: variableName | arrayElement | substring;

// R873 common-stmt -> COMMON [/ [common-block-name] /] common-block-object-list [[,] /
// [common-block-name] / common-block-object-list]...
commonStmt:
	'COMMON' ('/' commonBlockName? '/')? commonBlockObjectList (
		','? '/' commonBlockName? '/' commonBlockObjectList
	)*;

// R874 common-block-object -> variable-name [( array-spec )]
commonBlockObject: variableName ( '(' arraySpec ')')?;

// R901 designator -> object-name | array-element | array-section | coindexed-named-object |
// complex-part-designator | structure-component | substring
designator:
	objectName
	| arrayElement
	| arraySection
	| coindexedNamedObject
	| complexPartDesignator
	| structureComponent
	| substring;

// R902 variable -> designator | function-reference
variable: designator | functionReference;

// R903 variable-name -> name
variableName: name;

// R904 logical-variable -> variable
logicalVariable: variable;

// R905 char-variable -> variable
charVariable: variable;

// R906 default-char-variable -> variable
defaultCharVariable: variable;

// R907 int-variable -> variable
intVariable: variable;

// R908 substring -> parent-string ( substring-range )
substring: parentString '(' substringRange ')';

// R909 parent-string -> scalar-variable-name | array-element | coindexed-named-object |
// scalar-structure-component | scalar-char-literal-constant | scalar-named-constant
parentString:
	scalarVariableName
	| arrayElement
	| coindexedNamedObject
	| scalarStructureComponent
	| scalarCharLiteralConstant
	| scalarNamedConstant;

// R910 substring-range -> [scalar-int-expr] : [scalar-int-expr]
substringRange: scalarIntExpr? ':' scalarIntExpr?;

// R911 data-ref -> part-ref [% part-ref]...
dataRef: partRef ('%' partRef)*;

// R912 part-ref -> part-name [( section-subscript-list )] [image-selector]
partRef:
	partName ('(' sectionSubscriptList ')')? imageSelector?;

// R913 structure-component -> data-ref
structureComponent: dataRef;

// R914 coindexed-named-object -> data-ref
coindexedNamedObject: dataRef;

// R915 complex-part-designator -> designator % RE | designator % IM
complexPartDesignator: designator '%' ('RE' | 'IM');

// R916 type-param-inquiry -> designator % type-param-name
typeParamInquiry: designator '%' typeParamName;

// R917 array-element -> data-ref
arrayElement: dataRef;

// R918 array-section -> data-ref [( substring-range )] | complex-part-designator
arraySection:
	dataRef ('(' substringRange ')')?
	| complexPartDesignator;

// R919 subscript -> scalar-int-expr
subscript: scalarIntExpr;

// R920 section-subscript -> subscript | subscript-triplet | vector-subscript
sectionSubscript:
	subscript
	| subscriptTriplet
	| vectorSubscript;

// R921 subscript-triplet -> [subscript] : [subscript] [: stride]
subscriptTriplet: subscript? ':' subscript? (':' stride)?;

// R922 stride -> scalar-int-expr
stride: scalarIntExpr;

// R923 vector-subscript -> int-expr
vectorSubscript: intExpr;

// R924 image-selector -> lbracket cosubscript-list [, image-selector-spec-list] rbracket
imageSelector:
	lbracket cosubscriptList (',' imageSelectorSpecList)? rbracket;

// R925 cosubscript -> scalar-int-expr
cosubscript: scalarIntExpr;

// R926 image-selector-spec -> STAT = stat-variable | TEAM = team-value | TEAM_NUMBER =
// scalar-int-expr
imageSelectorSpec:
	'STAT' '=' statVariable
	| 'TEAM' '=' teamValue
	| 'TEAM_NUMBER' '=' scalarIntExpr;

// R927 allocate-stmt -> ALLOCATE ( [type-spec ::] allocation-list [, alloc-opt-list] )
allocateStmt:
	'ALLOCATE' '(' (typeSpec '::')? allocationList (
		',' allocOptList
	)? ')';

// R928 alloc-opt -> ERRMSG = errmsg-variable | MOLD = source-expr | SOURCE = source-expr | STAT =
// stat-variable
allocOpt:
	'ERRMSG' '=' errmsgVariable
	| 'MOLD' '=' sourceExpr
	| 'SOURCE' '=' sourceExpr
	| 'STAT' '=' statVariable;

// R929 stat-variable -> scalar-int-variable
statVariable: scalarIntVariable;

// R930 errmsg-variable -> scalar-default-char-variable
errmsgVariable: scalarDefaultCharVariable;

// R931 source-expr -> expr
sourceExpr: expr;

// R932 allocation -> allocate-object [( allocate-shape-spec-list )] [lbracket allocate-coarray-spec
// rbracket]
allocation:
	allocateObject ('(' allocateShapeSpecList ')')? (
		lbracket allocateCoarraySpec rbracket
	)?;

// R933 allocate-object -> variable-name | structure-component
allocateObject: variableName | structureComponent;

// R934 allocate-shape-spec -> [lower-bound-expr :] upper-bound-expr
allocateShapeSpec: (lowerBoundExpr ':')? upperBoundExpr;

// R935 lower-bound-expr -> scalar-int-expr
lowerBoundExpr: scalarIntExpr;

// R936 upper-bound-expr -> scalar-int-expr
upperBoundExpr: scalarIntExpr;

// R937 allocate-coarray-spec -> [allocate-coshape-spec-list ,] [lower-bound-expr :] *
allocateCoarraySpec:
	(allocateCoshapeSpecList ',')? (lowerBoundExpr ':')? '*';

// R938 allocate-coshape-spec -> [lower-bound-expr :] upper-bound-expr
allocateCoshapeSpec: (lowerBoundExpr ':')? upperBoundExpr;

// R939 nullify-stmt -> NULLIFY ( pointer-object-list )
nullifyStmt: 'NULLIFY' '(' pointerObjectList ')';

// R940 pointer-object -> variable-name | structure-component | proc-pointer-name
pointerObject:
	variableName
	| structureComponent
	| procPointerName;

// R941 deallocate-stmt -> DEALLOCATE ( allocate-object-list [, dealloc-opt-list] )
deallocateStmt:
	'DEALLOCATE' '(' allocateObjectList (',' deallocOptList)? ')';

// R942 dealloc-opt -> STAT = stat-variable | ERRMSG = errmsg-variable
deallocOpt: ('STAT' '=' statVariable)
	| ('ERRMSG' '=' errmsgVariable);

// R1001 primary -> literal-constant | designator | array-constructor | structure-constructor |
// function-reference | type-param-inquiry | type-param-name | ( expr )
primary:
	literalConstant
	| designator
	| arrayConstructor
	| structureConstructor
	| functionReference
	| typeParamInquiry
	| typeParamName
	| '(' expr ')';

// R1002 level-1-expr -> [defined-unary-op] primary
level1Expr: definedUnaryOp? primary;

// R1003 defined-unary-op -> . letter [letter]... .
definedUnaryOp: '.' letter+ '.';

// R1004 mult-operand -> level-1-expr [power-op mult-operand]
multOperand: level1Expr (powerOp multOperand)?;

// R1005 add-operand -> [add-operand mult-op] mult-operand
addOperand: (addOperand multOp)? multOperand;

// R1006 level-2-expr -> [[level-2-expr] add-op] add-operand
level2Expr: (level2Expr? addOp)? addOperand;

// R1007 power-op -> **
powerOp: '**';

// R1008 mult-op -> * | /
multOp: '*' | '/';

// R1009 add-op -> + | -
addOp: '+' | '-';

// R1010 level-3-expr -> [level-3-expr concat-op] level-2-expr
level3Expr: (level3Expr concatOp)? level2Expr;

// R1011 concat-op -> //
concatOp: '//';

// R1012 level-4-expr -> [level-3-expr rel-op] level-3-expr
level4Expr: (level3Expr relOp)? level3Expr;

// R1013 rel-op -> .EQ. | .NE. | .LT. | .LE. | .GT. | .GE. | == | /= | < | <= | > | >=
relOp:
	'.EQ.'
	| '.NE.'
	| '.LT.'
	| '.LE.'
	| '.GT.'
	| '.GE.'
	| '=='
	| '/='
	| '<'
	| '<='
	| '>'
	| '>=';

// R1014 and-operand -> [not-op] level-4-expr
andOperand: notOp? level4Expr;

// R1015 or-operand -> [or-operand and-op] and-operand
orOperand: (orOperand andOp)? andOperand;

// R1016 equiv-operand -> [equiv-operand or-op] or-operand
equivOperand: (equivOperand orOp)? orOperand;

// R1017 level-5-expr -> [level-5-expr equiv-op] equiv-operand
level5Expr: (level5Expr equivOp)? equivOperand;

// R1018 not-op -> .NOT.
notOp: '.NOT.';

// R1019 and-op -> .AND.
andOp: '.AND.';

// R1020 or-op -> .OR.
orOp: '.OR.';

// R1021 equiv-op -> .EQV. | .NEQV.
equivOp: '.EQV.' | '.NEQV.';

// R1022 expr -> [expr defined-binary-op] level-5-expr
expr: (expr definedBinaryOp)? level5Expr;

// R1023 defined-binary-op -> . letter [letter]... .
definedBinaryOp: '. letter' (letter)* '.';

// R1024 logical-expr -> expr
logicalExpr: expr;

// R1025 default-char-expr -> expr
defaultCharExpr: expr;

// R1026 int-expr -> expr
intExpr: expr;

// R1027 numeric-expr -> expr
numericExpr: expr;

// R1028 specification-expr -> scalar-int-expr
specificationExpr: scalarIntExpr;

// R1029 constant-expr -> expr
constantExpr: expr;

// R1030 default-char-constant-expr -> default-char-expr
defaultCharConstantExpr: defaultCharExpr;

// R1031 int-constant-expr -> int-expr
intConstantExpr: intExpr;

// R1032 assignment-stmt -> variable = expr
assignmentStmt: variable '=' expr;

// R1033 pointer-assignment-stmt -> data-pointer-object [( bounds-spec-list )] => data-target |
// data-pointer-object ( bounds-remapping-list ) => data-target | proc-pointer-object => proc-target
pointerAssignmentStmt:
	dataPointerObject ('(' boundsSpecList ')')? '=>' dataTarget
	| dataPointerObject '(' boundsRemappingList ')' '=>' dataTarget
	| procPointerObject '=>' procTarget;

// R1034 data-pointer-object -> variable-name | scalar-variable % data-pointer-component-name
dataPointerObject:
	variableName
	| scalarVariable '%' dataPointerComponentName;

// R1035 bounds-spec -> lower-bound-expr :
boundsSpec: lowerBoundExpr ':';

// R1036 bounds-remapping -> lower-bound-expr : upper-bound-expr
boundsRemapping: lowerBoundExpr ':' upperBoundExpr;

// R1037 data-target -> expr
dataTarget: expr;

// R1038 proc-pointer-object -> proc-pointer-name | proc-component-ref
procPointerObject: procPointerName | procComponentRef;

// R1039 proc-component-ref -> scalar-variable % procedure-component-name
procComponentRef: scalarVariable '%' procedureComponentName;

// R1040 proc-target -> expr | procedure-name | proc-component-ref
procTarget: expr | procedureName | procComponentRef;

// R1041 where-stmt -> WHERE ( mask-expr ) where-assignment-stmt
whereStmt: 'WHERE' '(' maskExpr ')' whereAssignmentStmt;

// R1042 where-construct -> where-construct-stmt [where-body-construct]... [masked-elsewhere-stmt
// [where-body-construct]...]... [elsewhere-stmt [where-body-construct]...] end-where-stmt
whereConstruct:
	whereConstructStmt (whereBodyConstruct)* (
		maskedElsewhereStmt (whereBodyConstruct)*
	)* (elsewhereStmt (whereBodyConstruct)*)* endWhereStmt;

// R1043 where-construct-stmt -> [where-construct-name :] WHERE ( mask-expr )
whereConstructStmt: (whereConstructName ':')? 'WHERE' '(' maskExpr ')';

// R1044 where-body-construct -> where-assignment-stmt | where-stmt | where-construct
whereBodyConstruct:
	whereAssignmentStmt
	| whereStmt
	| whereConstruct;

// R1045 where-assignment-stmt -> assignment-stmt
whereAssignmentStmt: assignmentStmt;

// R1046 mask-expr -> logical-expr
maskExpr: logicalExpr;

// R1047 masked-elsewhere-stmt -> ELSEWHERE ( mask-expr ) [where-construct-name]
maskedElsewhereStmt:
	'ELSEWHERE' '(' maskExpr ')' whereConstructName?;

// R1048 elsewhere-stmt -> ELSEWHERE [where-construct-name]
elsewhereStmt: 'ELSEWHERE' whereConstructName?;

// R1049 end-where-stmt -> END WHERE [where-construct-name]
endWhereStmt: 'END' 'WHERE' whereConstructName?;

// R1050 forall-construct -> forall-construct-stmt [forall-body-construct]... end-forall-stmt
forallConstruct:
	forallConstructStmt forallBodyConstruct* endForallStmt;

// R1051 forall-construct-stmt -> [forall-construct-name :] FORALL concurrent-header
forallConstructStmt: (forallConstructName ':')? 'FORALL' concurrentHeader;

// R1052 forall-body-construct -> forall-assignment-stmt | where-stmt | where-construct |
// forall-construct | forall-stmt
forallBodyConstruct:
	forallAssignmentStmt
	| whereStmt
	| whereConstruct
	| forallConstruct
	| forallStmt;

// R1053 forall-assignment-stmt -> assignment-stmt | pointer-assignment-stmt
forallAssignmentStmt: assignmentStmt | pointerAssignmentStmt;

// R1054 end-forall-stmt -> END FORALL [forall-construct-name]
endForallStmt: 'END' 'FORALL' forallConstructName?;

// R1055 forall-stmt -> FORALL concurrent-header forall-assignment-stmt
forallStmt: 'FORALL' concurrentHeader forallAssignmentStmt;

// R1101 block -> [execution-part-construct]...
block: (executionPartConstruct)*;

// R1102 associate-construct -> associate-stmt block end-associate-stmt
associateConstruct: associateStmt block endAssociateStmt;

// R1103 associate-stmt -> [associate-construct-name :] ASSOCIATE ( association-list )
associateStmt:
	(associateConstructName ':')? 'ASSOCIATE' '(' associationList ')';

// R1104 association -> associate-name => selector
association: associateName '=>' selector;

// R1105 selector -> expr | variable
selector: expr | variable;

// R1106 end-associate-stmt -> END ASSOCIATE [associate-construct-name]
endAssociateStmt: 'END' 'ASSOCIATE' associateConstructName?;

// R1107 block-construct -> block-stmt [block-specification-part] block end-block-stmt
blockConstruct:
	blockStmt blockSpecificationPart? block endBlockStmt;

// R1108 block-stmt -> [block-construct-name :] BLOCK
blockStmt: (blockConstructName ':')? 'BLOCK';

// R1109 block-specification-part -> [use-stmt]... [import-stmt]... [[declaration-construct]...
// specification-construct]
blockSpecificationPart:
	(useStmt)* (importStmt)* (
		(declarationConstruct)* specificationConstruct
	)?;

// R1110 end-block-stmt -> END BLOCK [block-construct-name]
endBlockStmt: 'END' 'BLOCK' blockConstructName?;

// R1111 change-team-construct -> change-team-stmt block end-change-team-stmt
changeTeamConstruct: changeTeamStmt block endChangeTeamStmt;

// R1112 change-team-stmt -> [team-construct-name :] CHANGE TEAM ( team-value [,
// coarray-association-list] [, sync-stat-list] )
changeTeamStmt: (teamConstructName ':')? 'CHANGE' 'TEAM' '(' teamValue (
		',' coarrayAssociationList
	)? (',' syncStatList)? ')';

// R1113 coarray-association -> codimension-decl => selector
coarrayAssociation: codimensionDecl '=>' selector;

// R1114 end-change-team-stmt -> END TEAM [( [sync-stat-list] )] [team-construct-name]
endChangeTeamStmt:
	'END' 'TEAM' ('(' syncStatList? ')')? teamConstructName?;

// R1115 team-value -> scalar-expr
teamValue: scalarExpr;

// R1116 critical-construct -> critical-stmt block end-critical-stmt
criticalConstruct: criticalStmt block endCriticalStmt;

// R1117 critical-stmt -> [critical-construct-name :] CRITICAL [( [sync-stat-list] )]
criticalStmt: (criticalConstructName ':')? 'CRITICAL' (
		'(' syncStatList? ')'
	)?;

// R1118 end-critical-stmt -> END CRITICAL [critical-construct-name]
endCriticalStmt: 'END' 'CRITICAL' criticalConstructName?;

// R1119 do-construct -> do-stmt block end-do
doConstruct: doStmt block endDo;

// R1120 do-stmt -> nonlabel-do-stmt | label-do-stmt
doStmt: nonlabelDoStmt | labelDoStmt;

// R1121 label-do-stmt -> [do-construct-name :] DO label [loop-control]
labelDoStmt: (doConstructName ':')? 'DO' label loopControl?;

// R1122 nonlabel-do-stmt -> [do-construct-name :] DO [loop-control]
nonlabelDoStmt: (doConstructName ':')? 'DO' loopControl?;

// R1123 loop-control -> [,] do-variable = scalar-int-expr , scalar-int-expr [, scalar-int-expr] |
// [,] WHILE ( scalar-logical-expr ) | [,] CONCURRENT concurrent-header concurrent-locality
loopControl:
	(',')? doVariable '=' scalarIntExpr ',' scalarIntExpr (
		',' scalarIntExpr
	)?
	| (',')? 'WHILE' '(' scalarLogicalExpr ')'
	| (',')? 'CONCURRENT' concurrentHeader concurrentLocality;

// R1124 do-variable -> scalar-int-variable-name
doVariable: scalarIntVariableName;

// R1125 concurrent-header -> ( [integer-type-spec ::] concurrent-control-list [, scalar-mask-expr]
// )
concurrentHeader:
	'(' (integerTypeSpec '::')? concurrentControlList (
		',' scalarMaskExpr
	)? ')';

// R1126 concurrent-control -> index-name = concurrent-limit : concurrent-limit [: concurrent-step]
concurrentControl:
	indexName '=' concurrentLimit ':' concurrentLimit (
		':' concurrentStep
	)?;

// R1127 concurrent-limit -> scalar-int-expr
concurrentLimit: scalarIntExpr;

// R1128 concurrent-step -> scalar-int-expr
concurrentStep: scalarIntExpr;

// R1129 concurrent-locality -> [locality-spec]...
concurrentLocality: (localitySpec)*;

// R1130 locality-spec -> LOCAL ( variable-name-list ) | LOCAL_INIT ( variable-name-list ) | SHARED
// ( variable-name-list ) | DEFAULT ( NONE )
localitySpec:
	'LOCAL' '(' variableNameList ')'
	| 'LOCAL_INIT' '(' variableNameList ')'
	| 'SHARED' '(' variableNameList ')'
	| 'DEFAULT' '(' 'NONE' ')';

// R1131 end-do -> end-do-stmt | continue-stmt
endDo: endDoStmt | continueStmt;

// R1132 end-do-stmt -> END DO [do-construct-name]
endDoStmt: 'END' 'DO' doConstructName?;

// R1133 cycle-stmt -> CYCLE [do-construct-name]
cycleStmt: 'CYCLE' doConstructName?;

// R1134 if-construct -> if-then-stmt block [else-if-stmt block]... [else-stmt block] end-if-stmt
ifConstruct:
	ifThenStmt block (elseIfStmt block)* (elseStmt block)? endIfStmt;

// R1135 if-then-stmt -> [if-construct-name :] IF ( scalar-logical-expr ) THEN
ifThenStmt: (ifConstructName ':')? 'IF' '(' scalarLogicalExpr ')' 'THEN';

// R1136 else-if-stmt -> ELSE IF ( scalar-logical-expr ) THEN [if-construct-name]
elseIfStmt:
	'ELSE' 'IF' '(' scalarLogicalExpr ')' 'THEN' ifConstructName?;

// R1137 else-stmt -> ELSE [if-construct-name]
elseStmt: 'ELSE' ifConstructName?;

// R1138 end-if-stmt -> END IF [if-construct-name]
endIfStmt: 'END' 'IF' ifConstructName?;

// R1139 if-stmt -> IF ( scalar-logical-expr ) action-stmt
ifStmt: 'IF' '(' scalarLogicalExpr ')' actionStmt;

// R1140 case-construct -> select-case-stmt [case-stmt block]... end-select-stmt
caseConstruct: selectCaseStmt (caseStmt block)* endSelectStmt;

// R1141 select-case-stmt -> [case-construct-name :] SELECT CASE ( case-expr )
selectCaseStmt: (caseConstructName ':')? 'SELECT' 'CASE' '(' caseExpr ')';

// R1142 case-stmt -> CASE case-selector [case-construct-name]
caseStmt: 'CASE' caseSelector caseConstructName?;

// R1143 end-select-stmt -> END SELECT [case-construct-name]
endSelectStmt: 'END' 'SELECT' caseConstructName?;

// R1144 case-expr -> scalar-expr
caseExpr: scalarExpr;

// R1145 case-selector -> ( case-value-range-list ) | DEFAULT
caseSelector: '(' caseValueRangeList ')' | 'DEFAULT';

// R1146 case-value-range -> case-value | case-value : | : case-value | case-value : case-value
caseValueRange:
	caseValue
	| caseValue ':'
	| ':' caseValue
	| caseValue ':' caseValue;

// R1147 case-value -> scalar-constant-expr
caseValue: scalarConstantExpr;

// R1148 select-rank-construct -> select-rank-stmt [select-rank-case-stmt block]...
// end-select-rank-stmt
selectRankConstruct:
	selectRankStmt (selectRankCaseStmt block)* endSelectRankStmt;

// R1149 select-rank-stmt -> [select-construct-name :] SELECT RANK ( [associate-name =>] selector )
selectRankStmt: (selectConstructName ':')? 'SELECT' 'RANK' '(' (
		associateName '=>'
	)? selector ')';

// R1150 select-rank-case-stmt -> RANK ( scalar-int-constant-expr ) [select-construct-name] | RANK (
// * ) [select-construct-name] | RANK DEFAULT [select-construct-name]
selectRankCaseStmt:
	'RANK' '(' scalarIntConstantExpr ')' selectConstructName?
	| 'RANK' '(' '*' ')' selectConstructName?
	| 'RANK' 'DEFAULT' selectConstructName?;

// R1151 end-select-rank-stmt -> END SELECT [select-construct-name]
endSelectRankStmt: 'END' 'SELECT' selectConstructName?;

// R1152 select-type-construct -> select-type-stmt [type-guard-stmt block]... end-select-type-stmt
selectTypeConstruct:
	selectTypeStmt (typeGuardStmt block)* endSelectTypeStmt;

// R1153 select-type-stmt -> [select-construct-name :] SELECT TYPE ( [associate-name =>] selector )
selectTypeStmt: (selectConstructName ':')? 'SELECT' 'TYPE' '(' (
		associateName '=>'
	)? selector ')';

// R1154 type-guard-stmt -> TYPE IS ( type-spec ) [select-construct-name] | CLASS IS (
// derived-type-spec ) [select-construct-name] | CLASS DEFAULT [select-construct-name]
typeGuardStmt:
	'TYPE' 'IS' '(' typeSpec ')' selectConstructName?
	| 'CLASS' 'IS' '(' derivedTypeSpec ')' selectConstructName?
	| 'CLASS' 'DEFAULT' selectConstructName?;

// R1155 end-select-type-stmt -> END SELECT [select-construct-name]
endSelectTypeStmt: 'END' 'SELECT' selectConstructName?;

// R1156 exit-stmt -> EXIT [construct-name]
exitStmt: 'EXIT' constructName?;

// R1157 goto-stmt -> GO TO label
gotoStmt: 'GO' 'TO' label;

// R1158 computed-goto-stmt -> GO TO ( label-list ) [,] scalar-int-expr
computedGotoStmt:
	'GO' 'TO' '(' labelList ')' ','? scalarIntExpr;

// R1159 continue-stmt -> CONTINUE
continueStmt: 'CONTINUE';

// R1160 stop-stmt -> STOP [stop-code] [, QUIET = scalar-logical-expr]
stopStmt: 'STOP' stopCode? (',' 'QUIET' '=' scalarLogicalExpr)?;

// R1161 error-stop-stmt -> ERROR STOP [stop-code] [, QUIET = scalar-logical-expr]
errorStopStmt:
	'ERROR' 'STOP' stopCode? (',' 'QUIET' '=' scalarLogicalExpr)?;

// R1162 stop-code -> scalar-default-char-expr | scalar-int-expr
stopCode: scalarDefaultCharExpr | scalarIntExpr;

// R1163 fail-image-stmt -> FAIL IMAGE
failImageStmt: 'FAIL' 'IMAGE';

// R1164 sync-all-stmt -> SYNC ALL [( [sync-stat-list] )]
syncAllStmt: 'SYNC' 'ALL' ('(' syncStatList ')')?;

// R1165 sync-stat -> STAT = stat-variable | ERRMSG = errmsg-variable
syncStat: 'STAT' '=' statVariable | 'ERRMSG' '=' errmsgVariable;

// R1166 sync-images-stmt -> SYNC IMAGES ( image-set [, sync-stat-list] )
syncImagesStmt:
	'SYNC' 'IMAGES' '(' imageSet (',' syncStatList)? ')';

// R1167 image-set -> int-expr | *
imageSet: intExpr | '*';

// R1168 sync-memory-stmt -> SYNC MEMORY [( [sync-stat-list] )]
syncMemoryStmt: 'SYNC' 'MEMORY' ('(' syncStatList? ')')?;

// R1169 sync-team-stmt -> SYNC TEAM ( team-value [, sync-stat-list] )
syncTeamStmt:
	'SYNC' 'TEAM' '(' teamValue (',' syncStatList)? ')';

// R1170 event-post-stmt -> EVENT POST ( event-variable [, sync-stat-list] )
eventPostStmt:
	'EVENT' 'POST' '(' eventVariable (',' syncStatList)? ')';

// R1171 event-variable -> scalar-variable
eventVariable: scalarVariable;

// R1172 event-wait-stmt -> EVENT WAIT ( event-variable [, event-wait-spec-list] )
eventWaitStmt:
	'EVENT' 'WAIT' '(' eventVariable (',' eventWaitSpecList)? ')';

// R1173 event-wait-spec -> until-spec | sync-stat
eventWaitSpec: untilSpec | syncStat;

// R1174 until-spec -> UNTIL_COUNT = scalar-int-expr
untilSpec: 'UNTIL_COUNT' '=' scalarIntExpr;

// R1175 form-team-stmt -> FORM TEAM ( team-number , team-variable [, form-team-spec-list] )
formTeamStmt:
	'FORM' 'TEAM' '(' teamNumber ',' teamVariable (
		',' formTeamSpecList
	)? ')';

// R1176 team-number -> scalar-int-expr
teamNumber: scalarIntExpr;

// R1177 team-variable -> scalar-variable
teamVariable: scalarVariable;

// R1178 form-team-spec -> NEW_INDEX = scalar-int-expr | sync-stat
formTeamSpec: 'NEW_INDEX' '=' scalarIntExpr | syncStat;

// R1179 lock-stmt -> LOCK ( lock-variable [, lock-stat-list] )
lockStmt: 'LOCK' '(' lockVariable (',' lockStatList)? ')';

// R1180 lock-stat -> ACQUIRED_LOCK = scalar-logical-variable | sync-stat
lockStat: 'ACQUIRED_LOCK' '=' scalarLogicalVariable | syncStat;

// R1181 unlock-stmt -> UNLOCK ( lock-variable [, sync-stat-list] )
unlockStmt: 'UNLOCK' '(' lockVariable (',' syncStatList)? ')';

// R1182 lock-variable -> scalar-variable
lockVariable: scalarVariable;

// R1201 io-unit -> file-unit-number | * | internal-file-variable
ioUnit: fileUnitNumber | '*' | internalFileVariable;

// R1202 file-unit-number -> scalar-int-expr
fileUnitNumber: scalarIntExpr;

// R1203 internal-file-variable -> char-variable
internalFileVariable: charVariable;

// R1204 open-stmt -> OPEN ( connect-spec-list )
openStmt: 'OPEN' '(' connectSpecList ')';

// R1205 connect-spec -> [UNIT =] file-unit-number | ACCESS = scalar-default-char-expr | ACTION =
// scalar-default-char-expr | ASYNCHRONOUS = scalar-default-char-expr | BLANK =
// scalar-default-char-expr | DECIMAL = scalar-default-char-expr | DELIM = scalar-default-char-expr
// | ENCODING = scalar-default-char-expr | ERR = label | FILE = file-name-expr | FORM =
// scalar-default-char-expr | IOMSG = iomsg-variable | IOSTAT = scalar-int-variable | NEWUNIT =
// scalar-int-variable | PAD = scalar-default-char-expr | POSITION = scalar-default-char-expr | RECL
// = scalar-int-expr | ROUND = scalar-default-char-expr | SIGN = scalar-default-char-expr | STATUS =
// scalar-default-char-expr
connectSpec:
	('UNIT' '=')? fileUnitNumber
	| 'ACCESS' '=' scalarDefaultCharExpr
	| 'ACTION' '=' scalarDefaultCharExpr
	| 'ASYNCHRONOUS' '=' scalarDefaultCharExpr
	| 'BLANK' '=' scalarDefaultCharExpr
	| 'DECIMAL' '=' scalarDefaultCharExpr
	| 'DELIM' '=' scalarDefaultCharExpr
	| 'ENCODING' '=' scalarDefaultCharExpr
	| 'ERR' '=' label
	| 'FILE' '=' fileNameExpr
	| 'FORM' '=' scalarDefaultCharExpr
	| 'IOMSG' '=' iomsgVariable
	| 'IOSTAT' '=' scalarIntVariable
	| 'NEWUNIT' '=' scalarIntVariable
	| 'PAD' '=' scalarDefaultCharExpr
	| 'POSITION' '=' scalarDefaultCharExpr
	| 'RECL' '=' scalarIntExpr
	| 'ROUND' '=' scalarDefaultCharExpr
	| 'SIGN' '=' scalarDefaultCharExpr
	| 'STATUS' '=' scalarDefaultCharExpr;

// R1206 file-name-expr -> scalar-default-char-expr
fileNameExpr: scalarDefaultCharExpr;

// R1207 iomsg-variable -> scalar-default-char-variable
iomsgVariable: scalarDefaultCharVariable;

// R1208 close-stmt -> CLOSE ( close-spec-list )
closeStmt: 'CLOSE' '(' closeSpecList ')';

// R1209 close-spec -> [UNIT =] file-unit-number | IOSTAT = scalar-int-variable | IOMSG =
// iomsg-variable | ERR = label | STATUS = scalar-default-char-expr
closeSpec:
	('UNIT' '=')? fileUnitNumber
	| 'IOSTAT' '=' scalarIntVariable
	| 'IOMSG' '=' iomsgVariable
	| 'ERR' '=' label
	| 'STATUS' '=' scalarDefaultCharExpr;

// R1210 read-stmt -> READ ( io-control-spec-list ) [input-item-list] | READ format [,
// input-item-list]
readStmt:
	'READ' '(' ioControlSpecList ')' (inputItemList)?
	| 'READ' format (',' inputItemList)?;

// R1211 write-stmt -> WRITE ( io-control-spec-list ) [output-item-list]
writeStmt: 'WRITE' '(' ioControlSpecList ')' (outputItemList)?;

// R1212 print-stmt -> PRINT format [, output-item-list]
printStmt: 'PRINT' format (',' outputItemList)?;

// R1213 io-control-spec -> [UNIT =] io-unit | [FMT =] format | [NML =] namelist-group-name |
// ADVANCE = scalar-default-char-expr | ASYNCHRONOUS = scalar-default-char-constant-expr | BLANK =
// scalar-default-char-expr | DECIMAL = scalar-default-char-expr | DELIM = scalar-default-char-expr
// | END = label | EOR = label | ERR = label | ID = id-variable | IOMSG = iomsg-variable | IOSTAT =
// scalar-int-variable | PAD = scalar-default-char-expr | POS = scalar-int-expr | REC =
// scalar-int-expr | ROUND = scalar-default-char-expr | SIGN = scalar-default-char-expr | SIZE =
// scalar-int-variable
ioControlSpec:
	('UNIT' '=')? ioUnit
	| ('FMT' '=')? format
	| ('NML' '=')? namelistGroupName
	| 'ADVANCE' '=' scalarDefaultCharExpr
	| 'ASYNCHRONOUS' '=' scalarDefaultCharConstantExpr
	| 'BLANK' '=' scalarDefaultCharExpr
	| 'DECIMAL' '=' scalarDefaultCharExpr
	| 'DELIM' '=' scalarDefaultCharExpr
	| 'END' '=' label
	| 'EOR' '=' label
	| 'ERR' '=' label
	| 'ID' '=' idVariable
	| 'IOMSG' '=' iomsgVariable
	| 'IOSTAT' '=' scalarIntVariable
	| 'PAD' '=' scalarDefaultCharExpr
	| 'POS' '=' scalarIntExpr
	| 'REC' '=' scalarIntExpr
	| 'ROUND' '=' scalarDefaultCharExpr
	| 'SIGN' '=' scalarDefaultCharExpr
	| 'SIZE' '=' scalarIntVariable;

// R1214 id-variable -> scalar-int-variable
idVariable: scalarIntVariable;

// R1215 format -> default-char-expr | label | *
format: defaultCharExpr | label | '*';

// R1216 input-item -> variable | io-implied-do
inputItem: variable | ioImpliedDo;

// R1217 output-item -> expr | io-implied-do
outputItem: expr | ioImpliedDo;

// R1218 io-implied-do -> ( io-implied-do-object-list , io-implied-do-control )
ioImpliedDo:
	'(' ioImpliedDoObjectList ',' ioImpliedDoControl ')';

// R1219 io-implied-do-object -> input-item | output-item
ioImpliedDoObject: inputItem | outputItem;

// R1220 io-implied-do-control -> do-variable = scalar-int-expr , scalar-int-expr [,
// scalar-int-expr]
ioImpliedDoControl:
	doVariable '=' scalarIntExpr ',' scalarIntExpr (
		',' scalarIntExpr
	)?;

// R1221 dtv-type-spec -> TYPE ( derived-type-spec ) | CLASS ( derived-type-spec )
dtvTypeSpec:
	'TYPE' '(' derivedTypeSpec ')'
	| 'CLASS' '(' derivedTypeSpec ')';

// R1222 wait-stmt -> WAIT ( wait-spec-list )
waitStmt: 'WAIT' '(' waitSpecList ')';

// R1223 wait-spec -> [UNIT =] file-unit-number | END = label | EOR = label | ERR = label | ID =
// scalar-int-expr | IOMSG = iomsg-variable | IOSTAT = scalar-int-variable
waitSpec:
	('UNIT' '=')? fileUnitNumber
	| 'END' '=' label
	| 'EOR' '=' label
	| 'ERR' '=' label
	| 'ID' '=' scalarIntExpr
	| 'IOMSG' '=' iomsgVariable
	| 'IOSTAT' '=' scalarIntVariable;

// R1224 backspace-stmt -> BACKSPACE file-unit-number | BACKSPACE ( position-spec-list )
backspaceStmt:
	'BACKSPACE' fileUnitNumber
	| 'BACKSPACE' '(' positionSpecList ')';

// R1225 endfile-stmt -> ENDFILE file-unit-number | ENDFILE ( position-spec-list )
endfileStmt:
	'ENDFILE' fileUnitNumber
	| 'ENDFILE' '(' positionSpecList ')';

// R1226 rewind-stmt -> REWIND file-unit-number | REWIND ( position-spec-list )
rewindStmt:
	'REWIND' fileUnitNumber
	| 'REWIND' '(' positionSpecList ')';

// R1227 position-spec -> [UNIT =] file-unit-number | IOMSG = iomsg-variable | IOSTAT =
// scalar-int-variable | ERR = label
positionSpec:
	('UNIT' '=')? fileUnitNumber
	| 'IOMSG' '=' iomsgVariable
	| 'IOSTAT' '=' scalarIntVariable
	| 'ERR' '=' label;

// R1228 flush-stmt -> FLUSH file-unit-number | FLUSH ( flush-spec-list )
flushStmt:
	'FLUSH' fileUnitNumber
	| 'FLUSH' '(' flushSpecList ')';

// R1229 flush-spec -> [UNIT =] file-unit-number | IOSTAT = scalar-int-variable | IOMSG =
// iomsg-variable | ERR = label
flushSpec:
	('UNIT' '=')? fileUnitNumber
	| 'IOSTAT' '=' scalarIntVariable
	| 'IOMSG' '=' iomsgVariable
	| 'ERR' '=' label;

// R1230 inquire-stmt -> INQUIRE ( inquire-spec-list ) | INQUIRE ( IOLENGTH = scalar-int-variable )
// output-item-list
inquireStmt:
	'INQUIRE' '(' inquireSpecList ')'
	| 'INQUIRE' '(' 'IOLENGTH' '=' scalarIntVariable ')' outputItemList;

// R1231 inquire-spec -> [UNIT =] file-unit-number | FILE = file-name-expr | ACCESS =
// scalar-default-char-variable | ACTION = scalar-default-char-variable | ASYNCHRONOUS =
// scalar-default-char-variable | BLANK = scalar-default-char-variable | DECIMAL =
// scalar-default-char-variable | DELIM = scalar-default-char-variable | ENCODING =
// scalar-default-char-variable | ERR = label | EXIST = scalar-logical-variable | FORM =
// scalar-default-char-variable | FORMATTED = scalar-default-char-variable | ID = scalar-int-expr |
// IOMSG = iomsg-variable | IOSTAT = scalar-int-variable | NAME = scalar-default-char-variable |
// NAMED = scalar-logical-variable | NEXTREC = scalar-int-variable | NUMBER = scalar-int-variable |
// OPENED = scalar-logical-variable | PAD = scalar-default-char-variable | PENDING =
// scalar-logical-variable | POS = scalar-int-variable | POSITION = scalar-default-char-variable |
// READ = scalar-default-char-variable | READWRITE = scalar-default-char-variable | RECL =
// scalar-int-variable | ROUND = scalar-default-char-variable | SEQUENTIAL =
// scalar-default-char-variable | SIGN = scalar-default-char-variable | SIZE = scalar-int-variable |
// STREAM = scalar-default-char-variable | STATUS = scalar-default-char-variable | WRITE =
// scalar-default-char-variable
inquireSpec:
	('UNIT' '=')? fileUnitNumber
	| 'FILE' '=' fileNameExpr
	| 'ACCESS' '=' scalarDefaultCharVariable
	| 'ACTION' '=' scalarDefaultCharVariable
	| 'ASYNCHRONOUS' '=' scalarDefaultCharVariable
	| 'BLANK' '=' scalarDefaultCharVariable
	| 'DECIMAL' '=' scalarDefaultCharVariable
	| 'DELIM' '=' scalarDefaultCharVariable
	| 'ENCODING' '=' scalarDefaultCharVariable
	| 'ERR' '=' label
	| 'EXIST' '=' scalarLogicalVariable
	| 'FORM' '=' scalarDefaultCharVariable
	| 'FORMATTED' '=' scalarDefaultCharVariable
	| 'ID' '=' scalarIntExpr
	| 'IOMSG' '=' iomsgVariable
	| 'IOSTAT' '=' scalarIntVariable
	| 'NAME' '=' scalarDefaultCharVariable
	| 'NAMED' '=' scalarLogicalVariable
	| 'NEXTREC' '=' scalarIntVariable
	| 'NUMBER' '=' scalarIntVariable
	| 'OPENED' '=' scalarLogicalVariable
	| 'PAD' '=' scalarDefaultCharVariable
	| 'PENDING' '=' scalarLogicalVariable
	| 'POS' '=' scalarIntVariable
	| 'POSITION' '=' scalarDefaultCharVariable
	| 'READ' '=' scalarDefaultCharVariable
	| 'READWRITE' '=' scalarDefaultCharVariable
	| 'RECL' '=' scalarIntVariable
	| 'ROUND' '=' scalarDefaultCharVariable
	| 'SEQUENTIAL' '=' scalarDefaultCharVariable
	| 'SIGN' '=' scalarDefaultCharVariable
	| 'SIZE' '=' scalarIntVariable
	| 'STREAM' '=' scalarDefaultCharVariable
	| 'STATUS' '=' scalarDefaultCharVariable
	| 'WRITE' '=' scalarDefaultCharVariable;

// R1301 format-stmt -> FORMAT format-specification
formatStmt: 'FORMAT' formatSpecification;

// R1302 format-specification -> ( [format-items] ) | ( [format-items ,] unlimited-format-item )
formatSpecification:
	'(' formatItems? ')'
	| '(' (formatItems ',')? unlimitedFormatItem ')';

// R1303 format-items -> format-item [[,] format-item]...
formatItems: formatItem (','? formatItem)*;

// R1304 format-item -> [r] data-edit-desc | control-edit-desc | char-string-edit-desc | [r] (
// format-items )
formatItem:
	'r'? dataEditDesc
	| controlEditDesc
	| charStringEditDesc
	| 'r'? '(' formatItems ')';

// R1305 unlimited-format-item -> * ( format-items )
unlimitedFormatItem: '*' '(' formatItems ')';

// R1306 r -> digit-string
r: digitString;

// R1307 data-edit-desc -> I w [. m] | B w [. m] | O w [. m] | Z w [. m] | F w . d | E w . d [E e] |
// EN w . d [E e] | ES w . d [E e] | EX w . d [E e] | G w [. d [E e]] | L w | A [w] | D w . d | DT
// [char-literal-constant] [( v-list )]
dataEditDesc:
	'I' w ('.' m)?
	| 'B' w ('.' m)?
	| 'O' w ('.' m)?
	| 'Z' w ('.' m)?
	| 'F' w '.' d
	| 'E' w '.' d ( 'E' e)?
	| 'EN' w '.' d ( 'E' e)?
	| 'ES' w '.' d ( 'E' e)?
	| 'EX' w '.' d ( 'E' e)?
	| 'G' w ('.' d ( 'E' e)?)?
	| 'L' w
	| 'A' w?
	| 'D' w '.' d
	| 'DT' charLiteralConstant? ( '(' vList ')')?;

// R1308 w -> digit-string
w: digitString;

// R1309 m -> digit-string
m: digitString;

// R1310 d -> digit-string
d: digitString;

// R1311 e -> digit-string
e: digitString;

// R1312 v -> [sign] digit-string
v: sign? digitString;

// R1313 control-edit-desc -> position-edit-desc | [r] / | : | sign-edit-desc | k P |
// blank-interp-edit-desc | round-edit-desc | decimal-edit-desc
controlEditDesc:
	positionEditDesc
	| 'r'? '/'
	| ':'
	| signEditDesc
	| 'k' 'P'
	| blankInterpEditDesc
	| roundEditDesc
	| decimalEditDesc;

// R1314 k -> [sign] digit-string
k: sign? digitString;

// R1315 position-edit-desc -> T n | TL n | TR n | n X
positionEditDesc: 'T' n | 'TL' n | 'TR' n | n 'X';

// R1316 n -> digit-string
n: digitString;

// R1317 sign-edit-desc -> SS | SP | S
signEditDesc: 'SS' | 'SP' | 'S';

// R1318 blank-interp-edit-desc -> BN | BZ
blankInterpEditDesc: 'BN' | 'BZ';

// R1319 round-edit-desc -> RU | RD | RZ | RN | RC | RP
roundEditDesc: 'RU' | 'RD' | 'RZ' | 'RN' | 'RC' | 'RP';

// R1320 decimal-edit-desc -> DC | DP
decimalEditDesc: 'DC' | 'DP';

// R1321 char-string-edit-desc -> char-literal-constant
charStringEditDesc: charLiteralConstant;

// R1401 main-program -> [program-stmt] [specification-part] [execution-part]
// [internal-subprogram-part] end-program-stmt
mainProgram:
	programStmt? specificationPart? executionPart? internalSubprogramPart? endProgramStmt;

// R1402 program-stmt -> PROGRAM program-name
programStmt: 'PROGRAM' programName;

// R1403 end-program-stmt -> END [PROGRAM [program-name]]
endProgramStmt: 'END' ('PROGRAM' programName?)?;

// R1404 module -> module-stmt [specification-part] [module-subprogram-part] end-module-stmt
module:
	moduleStmt specificationPart? moduleSubprogramPart? endModuleStmt;

// R1405 module-stmt -> MODULE module-name
moduleStmt: 'MODULE' moduleName;

// R1406 end-module-stmt -> END [MODULE [module-name]]
endModuleStmt: 'END' ('MODULE' moduleName?)?;

// R1407 module-subprogram-part -> contains-stmt [module-subprogram]...
moduleSubprogramPart: containsStmt moduleSubprogram*;

// R1408 module-subprogram -> function-subprogram | subroutine-subprogram |
// separate-module-subprogram
moduleSubprogram:
	functionSubprogram
	| subroutineSubprogram
	| separateModuleSubprogram;

// R1409 use-stmt -> USE [[, module-nature] ::] module-name [, rename-list] | USE [[, module-nature]
// ::] module-name , ONLY : [only-list]
useStmt:
	'USE' ((',' moduleNature)? '::')? moduleName (',' renameList)?
	| 'USE' ((',' moduleNature)? '::')? moduleName ',' 'ONLY' ':' onlyList?;

// R1410 module-nature -> INTRINSIC | NON_INTRINSIC
moduleNature: 'INTRINSIC' | 'NON_INTRINSIC';

// R1411 rename -> local-name => use-name | OPERATOR ( local-defined-operator ) => OPERATOR (
// use-defined-operator )
rename:
	localName '=>' useName
	| 'OPERATOR' '(' localDefinedOperator ')' '=>' 'OPERATOR' '(' useDefinedOperator ')';

// R1412 only -> generic-spec | only-use-name | rename
only: genericSpec | onlyUseName | rename;

// R1413 only-use-name -> use-name
onlyUseName: useName;

// R1414 local-defined-operator -> defined-unary-op | defined-binary-op
localDefinedOperator: definedUnaryOp | definedBinaryOp;

// R1415 use-defined-operator -> defined-unary-op | defined-binary-op
useDefinedOperator: definedUnaryOp | definedBinaryOp;

// R1416 submodule -> submodule-stmt [specification-part] [module-subprogram-part]
// end-submodule-stmt
submodule:
	submoduleStmt specificationPart? moduleSubprogramPart? endSubmoduleStmt;

// R1417 submodule-stmt -> SUBMODULE ( parent-identifier ) submodule-name
submoduleStmt:
	'SUBMODULE' '(' parentIdentifier ')' submoduleName;

// R1418 parent-identifier -> ancestor-module-name [: parent-submodule-name]
parentIdentifier: ancestorModuleName (':' parentSubmoduleName)?;

// R1419 end-submodule-stmt -> END [SUBMODULE [submodule-name]]
endSubmoduleStmt: 'END' ('SUBMODULE' submoduleName?)?;

// R1420 block-data -> block-data-stmt [specification-part] end-block-data-stmt
blockData: blockDataStmt specificationPart? endBlockDataStmt;

// R1421 block-data-stmt -> BLOCK DATA [block-data-name]
blockDataStmt: 'BLOCK' 'DATA' blockDataName?;

// R1422 end-block-data-stmt -> END [BLOCK DATA [block-data-name]]
endBlockDataStmt: 'END' ('BLOCK' 'DATA' blockDataName?)?;

// R1501 interface-block -> interface-stmt [interface-specification]... end-interface-stmt
interfaceBlock:
	interfaceStmt interfaceSpecification* endInterfaceStmt;

// R1502 interface-specification -> interface-body | procedure-stmt
interfaceSpecification: interfaceBody | procedureStmt;

// R1503 interface-stmt -> INTERFACE [generic-spec] | ABSTRACT INTERFACE
interfaceStmt:
	'INTERFACE' genericSpec?
	| 'ABSTRACT' 'INTERFACE';

// R1504 end-interface-stmt -> END INTERFACE [generic-spec]
endInterfaceStmt: 'END' 'INTERFACE' genericSpec?;

// R1505 interface-body -> function-stmt [specification-part] end-function-stmt | subroutine-stmt
// [specification-part] end-subroutine-stmt
interfaceBody:
	functionStmt specificationPart? endFunctionStmt
	| subroutineStmt specificationPart? endSubroutineStmt;

// R1506 procedure-stmt -> [MODULE] PROCEDURE [::] specific-procedure-list
procedureStmt:
	('MODULE')? 'PROCEDURE' '::'? specificProcedureList;

// R1507 specific-procedure -> procedure-name
specificProcedure: procedureName;

// R1508 generic-spec -> generic-name | OPERATOR ( defined-operator ) | ASSIGNMENT ( = ) |
// defined-io-generic-spec
genericSpec:
	genericName
	| 'OPERATOR' '(' definedOperator ')'
	| 'ASSIGNMENT' '(' '=' ')'
	| definedIOGenericSpec;

// R1509 defined-io-generic-spec -> READ ( FORMATTED ) | READ ( UNFORMATTED ) | WRITE ( FORMATTED )
// | WRITE ( UNFORMATTED )
definedIOGenericSpec:
	'READ' '(' 'FORMATTED' ')'
	| 'READ' '(' 'UNFORMATTED' ')'
	| 'WRITE' '(' 'FORMATTED' ')'
	| 'WRITE' '(' 'UNFORMATTED' ')';

// R1510 generic-stmt -> GENERIC [, access-spec] :: generic-spec => specific-procedure-list
genericStmt:
	'GENERIC' (',' accessSpec)? '::' genericSpec '=>' specificProcedureList;

// R1511 external-stmt -> EXTERNAL [::] external-name-list
externalStmt: 'EXTERNAL' '::'? externalNameList;

// R1512 procedure-declaration-stmt -> PROCEDURE ( [proc-interface] ) [[, proc-attr-spec]... ::]
// proc-decl-list
procedureDeclarationStmt:
	'PROCEDURE' '(' procInterface? ')' ((',' procAttrSpec)* '::')?;

// R1513 proc-interface -> interface-name | declaration-type-spec
procInterface: interfaceName | declarationTypeSpec;

// R1514 proc-attr-spec -> access-spec | proc-language-binding-spec | INTENT ( intent-spec ) |
// OPTIONAL | POINTER | PROTECTED | SAVE
procAttrSpec:
	accessSpec
	| procLanguageBindingSpec
	| 'INTENT' '(' intentSpec ')'
	| 'OPTIONAL'
	| 'POINTER'
	| 'PROTECTED'
	| 'SAVE';

// R1515 proc-decl -> procedure-entity-name [=> proc-pointer-init]
procDecl: procedureEntityName ('=>' procPointerInit)?;

// R1516 interface-name -> name
interfaceName: name;

// R1517 proc-pointer-init -> null-init | initial-proc-target
procPointerInit: nullInit | initialProcTarget;

// R1518 initial-proc-target -> procedure-name
initialProcTarget: procedureName;

// R1519 intrinsic-stmt -> INTRINSIC [::] intrinsic-procedure-name-list
intrinsicStmt: 'INTRINSIC' '::'? intrinsicProcedureNameList;

// R1520 function-reference -> procedure-designator ( [actual-arg-spec-list]
functionReference: procedureDesignator '(' (actualArgSpecList)?;

// R1521 call-stmt -> CALL procedure-designator [( [actual-arg-spec-list] )]
callStmt:
	'CALL' procedureDesignator ('(' (actualArgSpecList)? ')')?;

// R1522 procedure-designator -> procedure-name | proc-component-ref | data-ref % binding-name
procedureDesignator:
	procedureName
	| procComponentRef
	| dataRef '%' bindingName;

// R1523 actual-arg-spec -> [keyword =] actual-arg
actualArgSpec: (keyword '=')? actualArg;

// R1524 actual-arg -> expr | variable | procedure-name | proc-component-ref | alt-return-spec
actualArg:
	expr
	| variable
	| procedureName
	| procComponentRef
	| altReturnSpec;

// R1525 alt-return-spec -> * label
altReturnSpec: '*' label;

// R1526 prefix -> prefix-spec [prefix-spec]...
prefix: prefixSpec+;

// R1527 prefix-spec -> declaration-type-spec | ELEMENTAL | IMPURE | MODULE | NON_RECURSIVE | PURE |
// RECURSIVE
prefixSpec:
	declarationTypeSpec
	| 'ELEMENTAL'
	| 'IMPURE'
	| 'MODULE'
	| 'NON_RECURSIVE'
	| 'PURE'
	| 'RECURSIVE';

// R1528 proc-language-binding-spec -> language-binding-spec
procLanguageBindingSpec: languageBindingSpec;

// R1529 function-subprogram -> function-stmt [specification-part] [execution-part]
// [internal-subprogram-part] end-function-stmt
functionSubprogram:
	functionStmt (specificationPart)? (executionPart)? (
		internalSubprogramPart
	)? endFunctionStmt;

// R1530 function-stmt -> [prefix] FUNCTION function-name ( [dummy-arg-name-list] ) [suffix]
functionStmt:
	(prefix)? 'FUNCTION' functionName '(' (dummyArgNameList)? ')' (
		suffix
	)?;

// R1531 dummy-arg-name -> name
dummyArgName: name;

// R1532 suffix -> proc-language-binding-spec [RESULT ( result-name )] | RESULT ( result-name )
// [proc-language-binding-spec]
suffix:
	procLanguageBindingSpec ('RESULT' '(' resultName ')')?
	| 'RESULT' '(' resultName ')' procLanguageBindingSpec?;

// R1533 end-function-stmt -> END [FUNCTION [function-name]]
endFunctionStmt: 'END' ('FUNCTION' (functionName)?)?;

// R1534 subroutine-subprogram -> subroutine-stmt [specification-part] [execution-part]
// [internal-subprogram-part] end-subroutine-stmt
subroutineSubprogram:
	subroutineStmt (specificationPart)? (executionPart)? (
		internalSubprogramPart
	)? endSubroutineStmt;

// R1535 subroutine-stmt -> [prefix] SUBROUTINE subroutine-name [( [dummy-arg-list] )
// [proc-language-binding-spec]]
subroutineStmt:
	(prefix)? 'SUBROUTINE' subroutineName (
		'(' (dummyArgList)? ')'
	)? (procLanguageBindingSpec)?;

// R1536 dummy-arg -> dummy-arg-name | *
dummyArg: dummyArgName | '*';

// R1537 end-subroutine-stmt -> END [SUBROUTINE [subroutine-name]]
endSubroutineStmt: 'END' ('SUBROUTINE' (subroutineName)?)?;

// R1538 separate-module-subprogram -> mp-subprogram-stmt [specification-part] [execution-part]
// [internal-subprogram-part] end-mp-subprogram-stmt
separateModuleSubprogram:
	mpSubprogramStmt (specificationPart)? (executionPart)? (
		internalSubprogramPart
	)? endMpSubprogramStmt;

// R1539 mp-subprogram-stmt -> MODULE PROCEDURE procedure-name
mpSubprogramStmt: 'MODULE' 'PROCEDURE' procedureName;

// R1540 end-mp-subprogram-stmt -> END [PROCEDURE [procedure-name]]
endMpSubprogramStmt: 'END' ('PROCEDURE' (procedureName)?)?;

// R1541 entry-stmt -> ENTRY entry-name [( [dummy-arg-list] ) [suffix]]
entryStmt:
	'ENTRY' entryName ('(' (dummyArgList)? ')')? (suffix)?;

// R1542 return-stmt -> RETURN [scalar-int-expr]
returnStmt: 'RETURN' (scalarIntExpr)?;

// R1543 contains-stmt -> CONTAINS
containsStmt: 'CONTAINS';

// R1544 stmt-function-stmt -> function-name ( [dummy-arg-name-list] ) = scalar-expr
stmtFunctionStmt:
	functionName '(' (dummyArgNameList)? ')' '=' scalarExpr;
