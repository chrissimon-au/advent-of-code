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
Vector >> direction [
	^ direction
]

{ #category : #'as yet unclassified' }
Vector >> direction: dir [
	self assert: (#(#east, #west) includes: dir) description: dir, ' is not a valid direction.'.
	direction := dir
]

{ #category : #'as yet unclassified' }
Vector >> position [
	^ position
]

{ #category : #'as yet unclassified' }
Vector >> position: pos [
	position := pos
]
