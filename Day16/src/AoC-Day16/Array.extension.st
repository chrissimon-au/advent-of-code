Extension { #name : #Array }

{ #category : #'*AoC-Day16' }
Array >> asCoordinates [
	| size |
	size := self size.
	Transcript crShow: size.
	self
		assert: size = 2.
	^ Coordinates new
		col: (self at: 1);
		row: (self at: 2)
]
