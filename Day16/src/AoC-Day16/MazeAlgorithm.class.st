Class {
	#name : #MazeAlgorithm,
	#superclass : #AIGraphAlgorithm,
	#instVars : [
		'allPaths',
		'start',
		'end',
		'allNodesInShortestPaths',
		'shortestPath',
		'nodeAccumulator'
	],
	#category : #'AoC-Day16-Graph'
}

{ #category : #'as yet unclassified' }
MazeAlgorithm >> allNodesInShortestPaths [
	^allNodesInShortestPaths.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> allPaths [
	^allPaths.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> edgeClass [
	^AIWeightedEdge.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> end [
	^end.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> end: e [
	end := self findNode: e.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> findEdgeBetween:from and:to [
	^from outgoingEdges detect: [:e | e to = to].

]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> findPathsFrom: node distance: distance maxLength: maxDistance [ | removed |
	Transcript crShow: ('{1} | {2} | {3}' format: {node . distance . maxDistance}).
	(distance > maxDistance)
		ifTrue: [
			Transcript crShow: ('** Ending as {1} > {2}' format: {distance . maxDistance}).
			^true
		].
	(node = end)
		ifTrue: [
			Transcript crShow: '**** Ending as reached the end.'.
			allNodesInShortestPaths := nodeAccumulator copy.
			^true
		].
	(node visited)
		ifTrue: [
			Transcript crShow: '  Ending as already visited this node.'.
			^true
		].
	nodeAccumulator addLast: node model.
	node visited: true.
	node outgoingEdges do: [:e |
		Transcript crShow: ('    Trying with {1}' format: {e to}).
		self findPathsFrom: (e to) distance: (distance + e weight) maxLength: maxDistance.
	].
	removed :=	nodeAccumulator removeLast.
	Transcript crShow: ('   Removed {1}' format: {removed}).
	Transcript crShow: ('   CurrentPath {1}' format: {nodeAccumulator}).
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> nodeClass [
	^AIPathDistanceNode.
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
MazeAlgorithm >> run [ | graph shortestPathDistance|
	graph := AIDijkstra new.
	graph nodes: (self nodes collect: [:n | n model]).
	graph
		edges: (self edges collect: [:e | e model])
		from: #first
		to: #second
		weight: #third.

	graph start: start model.
	graph end: end model.
	graph run.
	
	shortestPathDistance := graph pathDistance.
	shortestPath := graph reconstructPath.
	nodeAccumulator := OrderedCollection new.
	
	Transcript crShow: ('Found shortest path with {1}' format: { shortestPathDistance }).
	
	self findPathsFrom: start distance: 0 maxLength: shortestPathDistance.
	
	^self.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> shortestPath [
	^shortestPath.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> start [
	^start.
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> start: s [
	start := self findNode: s.
]
