Class {
	#name : #MazeAlgorithm,
	#superclass : #AIGraphAlgorithm,
	#instVars : [
		'allPaths',
		'start',
		'end',
		'allNodesInShortestPaths',
		'shortestPath'
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
MazeAlgorithm >> findPathsFrom: node distance: distance maxLength: maxDistance [
	(distance > maxDistance)
		ifTrue: [^true].
	(node = end)
		ifTrue: [^true].
	(node visited)
		ifTrue: [^true].
	allNodesInShortestPaths add: node model.
	node visited.
	node outgoingEdges do: [:e | 
		self findPathsFrom: (e to) distance: (distance + e weight) maxLength: maxDistance.
	].
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
MazeAlgorithm >> run [ | graph minDistance|
	graph = AIDijkstra new.
	graph nodes: (self nodes collect: [:n | n model]).
	graph
		edges: (self edges collect: [:e | e model])
		from: #first
		to: #second
		weight: #third.

	graph start: start model.
	graph end: end model
	graph run.
	
	minDistance := graph pathDistance.
	shortestPath := graph reconstructPath.
	allNodesInShortestPaths := OrderedCollection new.
	
	self findPathsFrom: start distance: 0 maxLength: minDistance.
	
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
