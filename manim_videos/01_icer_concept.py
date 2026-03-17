"""
ICER Concept Explainer — Under 60 seconds
==========================================
Install: pip install manim
Run:     manim -pql 01_icer_concept.py ICERConcept
  -p  = preview after render
  -ql = low quality (480p, fast); use -qh for 1080p final render

Voiceover script (read aloud while video plays):
─────────────────────────────────────────────────
"When we compare two treatments, we look at two things —
how much more does the new treatment cost, and how much more
health does it deliver? The ICER — Incremental Cost-Effectiveness
Ratio — is simply the extra cost divided by the extra health gained,
usually measured in QALYs. If the ICER falls below the
willingness-to-pay threshold, the new treatment is considered
cost-effective."
─────────────────────────────────────────────────
"""

from manim import *

# Colour palette matching the workshop brochure
DEEP_BLUE = "#1B4965"
TEAL = "#028090"
MINT = "#02C39A"
AMBER = "#F0A500"
LIGHT_BG = "#F0F7F4"


class ICERConcept(Scene):
    def construct(self):
        self.camera.background_color = WHITE

        # ── Title (2s) ──
        title = Text("What is ICER?", font_size=48, color=DEEP_BLUE, weight=BOLD)
        subtitle = Text(
            "Incremental Cost-Effectiveness Ratio",
            font_size=24, color=TEAL
        ).next_to(title, DOWN, buff=0.3)

        self.play(Write(title), run_time=1)
        self.play(FadeIn(subtitle, shift=UP * 0.2), run_time=0.6)
        self.wait(0.8)
        self.play(FadeOut(title), FadeOut(subtitle), run_time=0.5)

        # ── Two treatments appear (3s) ──
        old_label = Text("Current\nTreatment", font_size=22, color=DEEP_BLUE).shift(LEFT * 4 + UP * 0.5)
        new_label = Text("New\nTreatment", font_size=22, color=TEAL).shift(RIGHT * 4 + UP * 0.5)

        old_box = SurroundingRectangle(old_label, color=DEEP_BLUE, buff=0.3, corner_radius=0.1)
        new_box = SurroundingRectangle(new_label, color=TEAL, buff=0.3, corner_radius=0.1)

        self.play(
            FadeIn(old_box), Write(old_label),
            FadeIn(new_box), Write(new_label),
            run_time=1
        )

        # Cost and QALY values
        old_cost = Text("Cost: ₹50,000", font_size=18, color="#555555").next_to(old_box, DOWN, buff=0.3)
        old_qaly = Text("QALYs: 3.0", font_size=18, color="#555555").next_to(old_cost, DOWN, buff=0.15)

        new_cost = Text("Cost: ₹80,000", font_size=18, color="#555555").next_to(new_box, DOWN, buff=0.3)
        new_qaly = Text("QALYs: 4.5", font_size=18, color="#555555").next_to(new_cost, DOWN, buff=0.15)

        self.play(
            FadeIn(old_cost), FadeIn(old_qaly),
            FadeIn(new_cost), FadeIn(new_qaly),
            run_time=1
        )
        self.wait(1)

        # ── Differences appear (4s) ──
        arrow = Arrow(
            old_box.get_right(), new_box.get_left(),
            color=AMBER, buff=0.2, stroke_width=4
        )
        delta_cost = MathTex(
            r"\Delta C = ₹30{,}000", font_size=30, color=AMBER
        ).next_to(arrow, UP, buff=0.15)
        delta_qaly = MathTex(
            r"\Delta E = 1.5 \text{ QALYs}", font_size=30, color=MINT
        ).next_to(arrow, DOWN, buff=0.15)

        self.play(GrowArrow(arrow), run_time=0.6)
        self.play(Write(delta_cost), run_time=0.7)
        self.play(Write(delta_qaly), run_time=0.7)
        self.wait(1)

        # ── Clear and show formula (5s) ──
        everything = VGroup(
            old_label, old_box, new_label, new_box,
            old_cost, old_qaly, new_cost, new_qaly,
            arrow, delta_cost, delta_qaly
        )
        self.play(FadeOut(everything), run_time=0.6)

        formula = MathTex(
            r"\text{ICER}", r"=",
            r"\frac{\Delta \text{Cost}}{\Delta \text{QALYs}}",
            r"=",
            r"\frac{₹30{,}000}{1.5}",
            r"=",
            r"₹20{,}000 \text{ per QALY}",
            font_size=36
        )
        formula.set_color_by_tex("ICER", DEEP_BLUE)
        formula.set_color_by_tex("Cost", AMBER)
        formula.set_color_by_tex("QALY", MINT)
        formula.set_color_by_tex("20", TEAL)

        self.play(Write(formula[:3]), run_time=1.2)
        self.wait(0.5)
        self.play(Write(formula[3:5]), run_time=1)
        self.wait(0.3)
        self.play(Write(formula[5:]), run_time=1)
        self.wait(1)

        # ── WTP threshold line (5s) ──
        self.play(formula.animate.shift(UP * 2), run_time=0.5)

        # Number line
        number_line = NumberLine(
            x_range=[0, 100000, 20000],
            length=10,
            include_numbers=True,
            include_tip=True,
            color=DEEP_BLUE,
            font_size=20,
        ).shift(DOWN * 0.5)

        nl_label = Text("₹ per QALY", font_size=18, color="#888888").next_to(number_line, DOWN, buff=0.3)
        self.play(Create(number_line), FadeIn(nl_label), run_time=1)

        # WTP threshold
        wtp_x = number_line.n2p(50000)
        wtp_line = DashedLine(
            start=wtp_x + DOWN * 0.6,
            end=wtp_x + UP * 1.2,
            color="#E74C3C",
            dash_length=0.1
        )
        wtp_label = Text("WTP Threshold\n₹50,000", font_size=16, color="#E74C3C").next_to(wtp_line, UP, buff=0.15)

        self.play(Create(wtp_line), FadeIn(wtp_label), run_time=0.8)

        # ICER dot
        icer_x = number_line.n2p(20000)
        icer_dot = Dot(icer_x, color=TEAL, radius=0.12)
        icer_label = Text("ICER\n₹20,000", font_size=16, color=TEAL).next_to(icer_dot, UP, buff=0.2)

        self.play(FadeIn(icer_dot, scale=2), FadeIn(icer_label), run_time=0.8)

        # Green zone
        green_zone = Rectangle(
            width=number_line.n2p(50000)[0] - number_line.n2p(0)[0],
            height=0.4,
            fill_color=MINT, fill_opacity=0.25,
            stroke_width=0
        ).move_to(
            (number_line.n2p(0) + number_line.n2p(50000)) / 2 + DOWN * 0.05
        )
        ce_label = Text("Cost-Effective", font_size=20, color=MINT, weight=BOLD).move_to(green_zone)

        self.play(FadeIn(green_zone), FadeIn(ce_label), run_time=0.8)
        self.wait(1)

        # ── Final takeaway (3s) ──
        takeaway = Text(
            "ICER < WTP threshold → New treatment is cost-effective",
            font_size=24, color=DEEP_BLUE, weight=BOLD
        ).to_edge(DOWN, buff=0.5)

        box = SurroundingRectangle(takeaway, color=MINT, buff=0.2, corner_radius=0.1, stroke_width=2)

        self.play(FadeIn(box), Write(takeaway), run_time=1)
        self.wait(2)
        self.play(*[FadeOut(mob) for mob in self.mobjects], run_time=0.5)
