Class {
	#name : #Day16Examples,
	#superclass : #Object,
	#category : #'AoC-Day16-Examples'
}

{ #category : #'as yet unclassified' }
Day16Examples >> checkTrue [
	<gtExample>
	| day16 |
	day16 := Day16 new.
	self assert: day16 check.
]
