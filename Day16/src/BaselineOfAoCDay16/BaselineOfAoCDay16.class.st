Class {
	#name : #BaselineOfAoCDay16,
	#superclass : #BaselineOf,
	#category : #BaselineOfAoCDay16
}

{ #category : #baseline }
BaselineOfAoCDay16 >> baseline: spec [
	<baseline>
		^ spec for: #common do: [
			spec package: 'AoC-Day16'
		]
]
