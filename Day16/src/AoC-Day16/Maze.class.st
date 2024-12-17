Class {
	#name : #Maze,
	#superclass : #Object,
	#instVars : [
		'reindeer',
		'end',
		'size',
		'walls',
		'graph'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #accessing }
Maze class >> parse: input [
	| maze rows |
	rows := input splitOn: Character cr.
	(rows size > 1)
		ifFalse: [
			rows := input splitOn: ('<n>' expandMacros).
		].
	
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
				].
			(cell = $#)
				ifTrue: [
					maze addWall: currentPos.
				]
		]
	].
	^maze.
]

{ #category : #'as yet unclassified' }
Maze >> addWall: wallPos [
	walls add: wallPos.
]

{ #category : #accessing }
Maze >> countSeatingTiles [
	^self seatingTiles size.
]

{ #category : #'as yet unclassified' }
Maze >> edges [
	^(self nodes flatCollect: [ :n | | next e |
		(n class = Vector)
			ifTrue: [
				e := 
				{
					{n . n rotateLeft . 1000}.
					{n . n rotateRight . 1000}.
				} asOrderedCollection.
				next := n next.	
				(self wallAt: next position)
					ifFalse:
						[ e add: ({n . next . 1}) ].
				( end = next position)
					ifTrue:
						[ e add: ({n . end . 1}) ].
				Transcript crShow: ('At {1}.  next is {2}. edges found are: {3}' format: {n . next .  e}).
				e
			]
			ifFalse: {}
	]) asOrderedCollection.
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
Maze >> graph [
	(graph = nil)
		ifTrue: [self run].
	^graph.
]

{ #category : #'as yet unclassified' }
Maze >> initialize [
	super initialize.
	walls := Set new.
]

{ #category : #accessing }
Maze >> isBranchPoint: position [ | east west south north countFree |
    east := position + (#(1 0) asCoordinates).
	west := position + (#(-1 0) asCoordinates).
	north := position + (#(0 -1) asCoordinates).
	south := position + (#(0 1) asCoordinates).
	countFree := 0.
"	Transcript crShow: ('      Counting free at ', position asString, ' looking at', north asString, east asString, south asString, west asString)."
	(self wallAt: east) ifFalse: [countFree := countFree +1].
	(self wallAt: west) ifFalse: [countFree := countFree +1].
	(self wallAt: north) ifFalse: [countFree := countFree +1].
	(self wallAt: south) ifFalse: [countFree := countFree +1].
"	Transcript crShow: ('         Free: ', countFree asString)."
	^(countFree > 2).
]

{ #category : #accessing }
Maze >> isPotentialBlockerAt: position [ | east west south north |
	east := position + (#(1 0) asCoordinates).
	west := position + (#(-1 0) asCoordinates).
	north := position + (#(0 -1) asCoordinates).
	south := position + (#(0 1) asCoordinates).
"	Transcript crShow: ('   Checking if ', position asString, ' is a blocker, looking at', north asString, east asString, south asString, west asString)."
	^((self isBranchPoint: east)
		or: [(self isBranchPoint: west)
		or: [(self isBranchPoint: north)
		or: [self isBranchPoint: south]]]).
	
]

{ #category : #'as yet unclassified' }
Maze >> lowestScore [
	(graph = nil)
		ifTrue: [self run].
	^graph pathDistance.
]

{ #category : #'as yet unclassified' }
Maze >> nodes [
	| n |
	n := ((1 to: size row) flatCollect: [ :r |
	(1 to: (size col)) flatCollect: [ :c | | currentPos |			
		currentPos := { c . r } asCoordinates.
		((self wallAt: currentPos) or: [currentPos = end])
			ifFalse:
			{
				Vector new position: currentPos; direction: #north.
				Vector new position: currentPos; direction: #south.
				Vector new position: currentPos; direction: #east.
				Vector new position: currentPos; direction: #west.
			}
			ifTrue: {}.
		].
	]) asOrderedCollection.
	n add: end.
	^n.
]

{ #category : #accessing }
Maze >> path [
	(graph = nil)
		ifTrue: [self run].
	^ self graph shortestPath.
]

{ #category : #'as yet unclassified' }
Maze >> positionsInPath [
	^ (self path
		collect: [ :p | p class = Vector ifTrue: [ p position ] ifFalse: [ p ] ]) asSet.
]

{ #category : #'as yet unclassified' }
Maze >> reindeer [
	^ reindeer
]

{ #category : #'as yet unclassified' }
Maze >> removeWall: wallPos [
	walls remove: wallPos.
]

{ #category : #'as yet unclassified' }
Maze >> render: path [
	^ Character cr
		join: ((1 to: size row)
				collect: [ :r | 
					''
						join: ((1 to: size col)
								collect: [ :c | 
									| cell currentPos |
									currentPos := {c.
											r} asCoordinates.
									cell := $..									
									cell := reindeer position = currentPos ifTrue: $S ifFalse: cell.
									cell := end = currentPos ifTrue: $E ifFalse: cell.
									cell := (self wallAt: currentPos) ifTrue: $# ifFalse: cell.
									cell := (path includes: currentPos) ifTrue: $O ifFalse: cell.
									cell ]) ])
]

{ #category : #'as yet unclassified' }
Maze >> reset [
	graph := nil.
]

{ #category : #'as yet unclassified' }
Maze >> run [
	graph := MazeAlgorithm new.
	graph nodes: self nodes.
	graph
		edges: self edges
		from: #first
		to: #second
		weight: #third.

	graph start: reindeer.
	graph end: end.
	graph run.
	^graph.
]

{ #category : #accessing }
Maze >> seatingTiles [
	(graph = nil)
		ifTrue: [self run].
	^graph allNodesInShortestPaths.
]

{ #category : #'as yet unclassified' }
Maze >> size [
	^ size.
]

{ #category : #accessing }
Maze >> size: s [
	size := s.
	1 to: (size col) do: [ :c |
		self addWall: {c . 1} asCoordinates.
		self addWall: {c . (size row)} asCoordinates.
	].
	1 to: size row do: [ :r |
		self addWall: {1 . r} asCoordinates.
		self addWall: {(size col) . r} asCoordinates.
	].

]

{ #category : #accessing }
Maze >> start: pos [
	reindeer := Vector new position: pos; direction: #east.
]

{ #category : #'as yet unclassified' }
Maze >> viewAllSolutions: aView [
	<gtView>
	^ aView textEditor
		title: 'All Solutions';
		fontName: 'Courier';
		text: (self render: (self seatingTiles))
]

{ #category : #'as yet unclassified' }
Maze >> viewMaze: aView [
	<gtView>
	^ aView textEditor
		title: 'Maze';
		fontName: 'Courier';
		text: (self render: (Set new))
]

{ #category : #'as yet unclassified' }
Maze >> viewSolvedMaze: aView [
	<gtView>
	^ aView textEditor
		title: 'Solved Maze';
		fontName: 'Courier';
		text: (self render: (self positionsInPath))
]

{ #category : #'as yet unclassified' }
Maze >> wallAt: coords [
	^ walls includes: coords.
]

{ #category : #'as yet unclassified' }
Maze >> walls [
	^walls.
]
