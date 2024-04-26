{
  "branches": ["main"],
  "plugins": [
    [
      "@semantic-release/commit-analyzer",
      {
        "preset": "conventionalcommits",
        "releaseRules": [
		  { "type": "build", "release": "patch" },
          { "type": "refactor", "release": "patch" },
          { "type": "style", "release": "none" },
		  { "type": "test", "release": "patch" },
          { "type": "style", "release": "none" },
		  { "type": "chore", "release": "none" },
		  { "type": "ci", "release": "none" }
        ],
      }
    ],
  ]
}
