Class {
	#name : #Coordinates,
	#superclass : #Object,
	#instVars : [
		'col',
		'row'
	],
	#category : #'AoC-Day16-Model'
}

{ #category : #accessing }
Coordinates >> col [
	^col.
]

{ #category : #accessing }
Coordinates >> col: anInteger [
	col := anInteger.
]

{ #category : #accessing }
Coordinates >> row [
	^row.
]

{ #category : #accessing }
Coordinates >> row: anInteger [ 
	row := anInteger.
]
