Class {
	#name : #MazeAlgorithm,
	#superclass : #AIDijkstra,
	#instVars : [
		'allPaths'
	],
	#category : #'AoC-Day16-Graph'
}

{ #category : #'as yet unclassified' }
MazeAlgorithm >> allPaths [
	^allPaths.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> findEdgeBetween:from and:to [
	^from outgoingEdges detect: [:e | e to = to].

]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> reconstructPathEdges [
	| path previous previousEdge|
	"If no path exists between the start and the end node"
	end pathDistance = Float infinity ifTrue: [ ^ #(  ) ].
	path := LinkedList empty.
	previous := end.
	previousEdge := self findEdgeBetween: previous previousNode and: previous.
	path addFirst: previousEdge.
	[ previous previousNode = start ] whileFalse: [
		previous := previous previousNode.
		previousEdge := self findEdgeBetween: previous previousNode and: previous.
		path addFirst: previousEdge ].
	^ path
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> run [ | minDistance pathEdges |
	allPaths := OrderedCollection new.
	super run.
	
	allPaths add: {#path -> (self reconstructPath) . #distance -> (self pathDistance)}.
	
	minDistance := self pathDistance.
	pathEdges := self reconstructPathEdges.
		
	Transcript crShow: ('Found path with minDistance:{1} and edgeCount:{2}.' format: {minDistance . pathEdges size}).
	Transcript crShow: ''.
	"Try finding shortest path for each version of the graph with one of the shortest path edges missing.  Any versions that have the same mindistance are kept."
	pathEdges do: [:edge | | graph distance|
		graph := AIDijkstra new.
		Transcript crShow: ('Experimenting with removing edge {1} using graph {2}' format: {edge . graph class}).
		graph nodes: (self nodes collect: [:n | n model]).
		graph
			edges: ((self edges reject: [:e|e = edge]) collect: [:e | e model])
			from: #first
			to: #second
			weight: #third.
		graph start: (start model).
		graph end: (end model).
		graph run.
		
		distance := graph pathDistance.
		Transcript crShow: ('   Found path with distance {1}.' format: {distance}).
		(distance = minDistance) ifTrue: [
			allPaths add: {#path -> (graph reconstructPath) . #distance -> distance}.
		].
	].
	^ self.
	
]
