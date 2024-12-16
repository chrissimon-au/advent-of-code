Class {
	#name : #MazeExamples,
	#superclass : #Object,
	#category : #'AoC-Day16-Examples'
}

{ #category : #accessing }
MazeExamples class >> singleTurn [
	<gtExample>
	| maze |
	maze := Maze new size: #(8 4) asCoordinates; start: #(2 2) asCoordinates; end: #(7 3) asCoordinates.
	self assert: maze lowestScore equals: (7 + 1000).
	^maze
]

{ #category : #accessing }
MazeExamples class >> straightLine [
	<gtExample>
	| maze |
	maze := Maze new size: #(8 3) asCoordinates; start: #(2 2) asCoordinates; end:#(7 2) asCoordinates.
	self assert: maze size col equals: 8.
	self assert: maze size row equals: 3.
	self assert: maze lowestScore equals: 5.
	^maze
]
