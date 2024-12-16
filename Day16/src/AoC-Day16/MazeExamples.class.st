Class {
	#name : #MazeExamples,
	#superclass : #Object,
	#category : #'AoC-Day16-Examples'
}

{ #category : #accessing }
MazeExamples class >> parsed [
	<gtExample>
	| maze |
	maze := Maze parse: ('
########
#.S.#..#
#.....E#
########
') trim.
	self assert: maze size row equals: 4.
	self assert: maze size col equals: 8.
	self assert: maze reindeer position col equals: 3.
	self assert: maze reindeer position row equals: 2.
	self assert: maze reindeer direction equals: #east.
	self assert: maze end col equals: 7.
	self assert: maze end row equals: 3.
	self assert: (maze wallAt: #(5 2) asCoordinates).
	^ maze.
]

{ #category : #accessing }
MazeExamples class >> singleTurn [
	<gtExample>
	| maze |
	maze := Maze new size: #(8 4) asCoordinates; start: #(2 2) asCoordinates; end: #(7 3) asCoordinates.
	self assert: maze lowestScore equals: (6 + 1000).
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
