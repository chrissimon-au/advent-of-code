Class {
	#name : #MazeAlgorithm,
	#superclass : #AIDijkstra,
	#category : #'AoC-Day16-Graph'
}

{ #category : #'as yet unclassified' }
MazeAlgorithm >> addPreviousNodeTo: aNode previousNode: previousNode [
	aNode previousNodes add: previousNode
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> allNodesInShortestPaths [
	| allNodes collectNodes|
	"If no path exists between the start and the end node"
	end pathDistance = Float infinity ifTrue: [ ^ #(  ) ].
	allNodes := Set empty.
	allNodes add: end model.
	
	collectNodes := [ :n |
		n previousNodes do: [ :pn |
			allNodes add: pn model.
			collectNodes value: pn.
		].
	].
	
	collectNodes value: end.
	^ allNodes.
]

{ #category : #configuration }
MazeAlgorithm >> nodeClass [

	^ MultiPathNode
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> run [
	| pq |
	pq := self newPriorityQueue.
	pq add: start -> 0.

	[ pq isNotEmpty ] whileTrue: [
		| assoc node minWeight |
		assoc := self removeMostPromisingPair: pq.
		node := assoc key.
		minWeight := assoc value.
		node visited: true.

		"Skip if the path weight is less than the one obtained from the pq.
		This is an optimization for not processing unnecessary nodes."
		node pathDistance < minWeight ifFalse: [
			node outgoingEdges do: [ :edge |
				edge to visited ifFalse: [
					| newDistance |
					newDistance := node pathDistance + edge weight.

					newDistance = edge to pathDistance ifTrue: [
						self addPreviousNodeTo: edge to previousNode: node.
					]
					ifFalse: [
						newDistance < edge to pathDistance ifTrue: [
							self updateDistance: newDistance of: edge to previousNode: node.
							pq add: edge to -> newDistance ]
					]
				]
			]
		]
	]
]

{ #category : #'as yet unclassified' }
MazeAlgorithm >> shortestPath [
	^self reconstructPath.
]

{ #category : #accessing }
MazeAlgorithm >> updateDistance: newDistance of: aNode previousNode: previousNode [
	super updateDistance: newDistance of: aNode previousNode: previousNode.
	aNode clearPreviousNodes.
	self addPreviousNodeTo: aNode previousNode: previousNode.
]
