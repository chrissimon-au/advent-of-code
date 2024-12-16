Class {
	#name : #Vector,
	#superclass : #Object,
	#instVars : [
		'direction',
		'position'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #'as yet unclassified' }
Vector >> = other [
	self class = other class
		ifFalse: [ ^ false ].
	^direction = other direction and: [position = other position].
]

{ #category : #'as yet unclassified' }
Vector >> asString [
	^ position asString, ':', direction asString.
]

{ #category : #'as yet unclassified' }
Vector >> direction [
	^ direction
]

{ #category : #'as yet unclassified' }
Vector >> direction: dir [
	self assert: (#(#east #west #north #south) includes: dir) description: dir, ' is not a valid direction.'.
	direction := dir
]

{ #category : #'as yet unclassified' }
Vector >> gtDisplayOn: aStream [
	aStream nextPutAll: (self asString).
]

{ #category : #'as yet unclassified' }
Vector >> hash [
	^direction hash bitXor: position hash.
]

{ #category : #'as yet unclassified' }
Vector >> next [
	|movement|
	movement := 
		(direction = #east) ifTrue: #(1 0) ifFalse: movement. 
	movement := 
		(direction = #west) ifTrue: #(-1 0) ifFalse: movement.
	movement := 
		(direction = #north) ifTrue: #(0 -1) ifFalse: movement.
	movement := 
		(direction = #south) ifTrue: #(0 1) ifFalse: movement.
	^Vector new position: (position + movement asCoordinates); direction: direction.
	
]

{ #category : #'as yet unclassified' }
Vector >> position [
	^ position
]

{ #category : #'as yet unclassified' }
Vector >> position: pos [
	position := pos
]

{ #category : #'as yet unclassified' }
Vector >> printOn: aStream [
	aStream nextPutAll: (self asString).
]

{ #category : #'as yet unclassified' }
Vector >> rotateLeft [
	| next_dir |
	next_dir := 
		(direction = #east) ifTrue: #north ifFalse: next_dir. 
	next_dir := 
		(direction = #west) ifTrue: #south ifFalse: next_dir.
	next_dir := 
		(direction = #north) ifTrue: #west ifFalse: next_dir.
	next_dir := 
		(direction = #south) ifTrue: #east ifFalse: next_dir.
	^Vector new position: position; direction: next_dir.
]

{ #category : #'as yet unclassified' }
Vector >> rotateRight [
	| next_dir |
	next_dir := 
		(direction = #east) ifTrue: #south ifFalse: next_dir. 
	next_dir := 
		(direction = #west) ifTrue: #north ifFalse: next_dir.
	next_dir := 
		(direction = #north) ifTrue: #east ifFalse: next_dir.
	next_dir := 
		(direction = #south) ifTrue: #west ifFalse: next_dir.
	^Vector new position: position; direction: next_dir.
]
