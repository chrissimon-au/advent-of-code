Class {
	#name : #Movement,
	#superclass : #Object,
	#instVars : [
		'start',
		'end',
		'cost'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #'as yet unclassified' }
Movement >> asString [
	^ 'From [', start asString, '] to [', end asString, '] costing ', cost asString, '.'.
]

{ #category : #'as yet unclassified' }
Movement >> cost [
	^cost.
]

{ #category : #'as yet unclassified' }
Movement >> cost: c [
	cost := c.
]

{ #category : #'as yet unclassified' }
Movement >> end [
	^end.
]

{ #category : #'as yet unclassified' }
Movement >> end: e [
	end := e.
]

{ #category : #'as yet unclassified' }
Movement >> gtDisplayOn: aStream [
	aStream nextPutAll: (self asString) 
]

{ #category : #'as yet unclassified' }
Movement >> printOn: aStream [
	aStream nextPutAll: (self asString) 
]

{ #category : #'as yet unclassified' }
Movement >> start [
	^start.
]

{ #category : #'as yet unclassified' }
Movement >> start: s [
	start := s.
]
