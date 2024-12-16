Class {
	#name : #MazeExamples,
	#superclass : #Object,
	#category : #'AoC-Day16-Examples'
}

{ #category : #accessing }
MazeExamples class >> singleTurn [
	<gtExample>
	| maze |
	maze := Maze new width: 8; height: 4; start: (Position new col: 2; row: 2); end: (Position new col: 7; row: 3).
	self assert: maze lowestScore equals: (7 + 1000).
	^maze
]

{ #category : #accessing }
MazeExamples class >> straightLine [
	<gtExample>
	| maze |
	maze := Maze new width: 8; height: 3; start: (Position new col: 2; row: 2); end: (Position new col: 7; row: 2).
	Transcript crShow: maze width.
	self assert: maze width equals: 8.
	self assert: maze height equals: 3.
	self assert: maze lowestScore equals: 5.
	^maze
]
