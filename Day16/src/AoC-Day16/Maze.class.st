Class {
	#name : #Maze,
	#superclass : #Object,
	#instVars : [
		'reindeer',
		'end',
		'size'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #accessing }
Maze class >> parse: input [
	| maze rows |
	rows := input splitOn: Character cr.
	
	maze := Maze new 
		size: {(rows at: 1) size . rows size } asCoordinates.
	
	rows doWithIndex: [ :row :rowIdx |
		row doWithIndex: [ :cell :colIdx | | currentPos |
			currentPos := {colIdx . rowIdx } asCoordinates.
			(cell = $S)
				ifTrue: [ 
					maze start: currentPos.
				].
			(cell = $E)
				ifTrue: [
					maze end: currentPos.
				]
		]
	].
	^maze.
]

{ #category : #'as yet unclassified' }
Maze >> end [
	^end.
]

{ #category : #accessing }
Maze >> end: pos [
	end := pos
]

{ #category : #'as yet unclassified' }
Maze >> lowestScore [
	^ size row > 3
		ifTrue: 1007
		ifFalse: 5
]

{ #category : #'as yet unclassified' }
Maze >> reindeer [
	^ reindeer
]

{ #category : #'as yet unclassified' }
Maze >> size [
	^ size.
]

{ #category : #accessing }
Maze >> size: s [
	size := s
]

{ #category : #accessing }
Maze >> start: pos [
	reindeer := Vector new position: pos; direction: #east.
]

{ #category : #'as yet unclassified' }
Maze >> viewMaze: aView [
	<gtView>
	| renderedMaze |
	renderedMaze := Character cr join: ((1 to: size row) collect: [ :r |
		'' join: ((1 to: (size col)) collect: [ :c | | cell |
			cell := $..
			cell := (r = 1 | (r = size row))
				ifTrue: $#
				ifFalse: cell.
			cell := (c = 1 | (c = size col))
				ifTrue: $#
				ifFalse: cell.
			cell := (c = reindeer position col & (r = reindeer position row))
				ifTrue: $S
				ifFalse: cell.
			cell := (c = end col & (r = end row))
				ifTrue: $E
				ifFalse: cell.
			cell
		]).
	]).	
	^ aView textEditor
		title: 'Maze';
		fontName: 'Courier';
		text: renderedMaze.
]
