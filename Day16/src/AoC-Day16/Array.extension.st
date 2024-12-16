Extension { #name : #Array }

{ #category : #'*AoC-Day16' }
Array >> asCoordinates [
	| size |
	size := self size.
	self
		assert: size = 2.
	^ Coordinates new
		col: (self at: 1);
		row: (self at: 2)
]

{ #category : #'*AoC-Day16' }
Array >> asMovement [
	| size |
	size := self size.
	self
		assert: size = 3.
	^ Movement new
		start: (self at: 1);
		end: (self at: 2);
		cost: (self at: 3).
]
