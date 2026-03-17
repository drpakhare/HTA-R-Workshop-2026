# HTA Concept Explainer Videos — Manim Scripts

## Setup (one-time)

```bash
# Install Manim (requires Python 3.8+)
pip install manim

# On Mac, also need:
brew install cairo pango ffmpeg

# On Windows, install ffmpeg and add to PATH
```

## Rendering a video

```bash
# Quick preview (480p, fast)
manim -pql 01_icer_concept.py ICERConcept

# Final render (1080p)
manim -pqh 01_icer_concept.py ICERConcept

# Output goes to: media/videos/01_icer_concept/1080p60/ICERConcept.mp4
```

## Adding voiceover

After rendering, combine with audio using:
```bash
ffmpeg -i ICERConcept.mp4 -i voiceover.mp3 -c:v copy -c:a aac -shortest output.mp4
```

Or use OBS / any screen recorder to narrate while the video plays.

## Video list

| # | Concept | Script | Duration |
|---|---------|--------|----------|
| 1 | ICER | 01_icer_concept.py | ~50s |
| 2 | CEA | (coming soon) | |
| 3 | QALYs & Utilities | (coming soon) | |
| 4 | Decision Tree Logic | (coming soon) | |
| 5 | Health States & Transitions | (coming soon) | |
| 6 | Discounting | (coming soon) | |
| 7 | WTP Threshold | (coming soon) | |
| 8 | Sensitivity Analysis | (coming soon) | |
| 9 | Time Horizon & Cycle Length | (coming soon) | |
| 10 | Perspective in HTA | (coming soon) | |
