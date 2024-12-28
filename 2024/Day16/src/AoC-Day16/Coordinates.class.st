Class {
	#name : #Coordinates,
	#superclass : #Object,
	#instVars : [
		'col',
		'row'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #'as yet unclassified' }
Coordinates >> + other [
	|res|
	^{ col + (other col) . row + (other row) } asCoordinates.
]

{ #category : #'as yet unclassified' }
Coordinates >> = anOther [
	anOther class = self class
		ifFalse: [ ^ false ].
	^anOther col = col and: [anOther row = row].
]

{ #category : #'as yet unclassified' }
Coordinates >> asString [
	^'(', col asString, ',', row asString, ')'.
]

{ #category : #accessing }
Coordinates >> col [
	^col.
]

{ #category : #accessing }
Coordinates >> col: anInteger [
	col := anInteger.
]

{ #category : #'as yet unclassified' }
Coordinates >> gtDisplayOn: aStream [
	aStream nextPutAll: (self asString).
]

{ #category : #'as yet unclassified' }
Coordinates >> hash [
	^col hash bitXor: row hash.
	
]

{ #category : #'as yet unclassified' }
Coordinates >> position [
	^self.
]

{ #category : #'as yet unclassified' }
Coordinates >> printOn: aStream [
	aStream nextPutAll: (self asString) 
]

{ #category : #accessing }
Coordinates >> row [
	^row.
]

{ #category : #accessing }
Coordinates >> row: anInteger [ 
	row := anInteger.
]
