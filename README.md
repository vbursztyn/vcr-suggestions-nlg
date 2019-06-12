## Sketch-based Services: Visual-Conceptual Relationship Suggestions in Natural Language.
## Student: Victor S. Bursztyn (v-bursztyn@u.northwestern.edu).

### Background:
Sketches often convey conceptual relationships between entities via the visual relationships between their depictions (glyphs). In CogSketch, as users sketch and provide conceptual information about their glyphs, the underlying reasoning system looks for suggestions of visual-conceptual relationships (VCR) based on prior experience (refer to subsection 3.3 in "Analogical Learning of Visual/Conceptual Relationships in Sketches" by Forbus, Usher, and Tomei, 2005). These VCR suggestions are then presented to users, who ultimately decide which ones to retrieve. However, suggestions are presented in their internal representations rather than in one that is natural to most users. To address this limitation, I worked on converting VCR suggestions to natural language using Verbalize, FIRE's natural language generation (NLG) module. In this process, I found two opportunities for filtering information: (i) by identifying and handling suggestions that are overspecialized and cannot be concisely explained to users; and (ii) by filtering suggestions that are semantically equivalent due to the symmetric property of a relation (e.g., if A is adjacent to B, then B is adjacent to A — therefore, these alternatives are redundant).

### Key Features:
1. VCR suggestions integrated with Verbalize for the two visual relationships with the most specializations: insideInSketch (17 specializations) and atOrOverlapsInSketch (43 specializations).
2. Additional integration with Multimodal Interpretation Group (12 additional relations).
3. Absence of genFormat returns nil, serving as a filter for suggestions that cannot be clearly/concisely explained.
4. Symmetric relations are handled in order to filter redundant suggestions (e.g., “Is A adjacent to B?” appearing once).

### Enclosed Files:
1. "vcr-question-form.lsp"
2. "cogsketch-vcr-nlg.krf"
3. "sketch_services_results.pdf"

### Software Artifacts:
1. Lisp Code: function filter-symmetric-relns (lines 194-206 in vcr-question-form.lsp) has a filter that is used in function display-vcr-questions (lines 179-191).
2. Lisp Code: function print-vcr-reln-select (lines 339-359 in vcr-question-form.lsp) is modified to used Verbalize.
3. Knowledge Representation File: One Micro-Theory holding 60 genFormats for 60 distinct visual-conceptual relationships, formatted as questions.

### Integration Steps:
1. Load file "cogsketch-vcr-nlg.krf" into the Knowledge Base.
2. Replace the old "vcr-question-form.lsp" in "your_path_to_cogsketch_source\v4\rbrowse" by the new one.
3. Recompile CogSketch, run it, and create a new sketch. The new VCR suggestions should be displayed as in "sketch_services_results.pdf" (slide 3).