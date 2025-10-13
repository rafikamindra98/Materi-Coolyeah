import pygame
import random
import sys
import math
import time
from enum import Enum

# Initialize Pygame
pygame.init()

# Constants
WINDOW_WIDTH = 1000
WINDOW_HEIGHT = 700
FPS = 60

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
DARK_BLUE = (26, 26, 46)
BLUE = (52, 152, 219)
RED = (231, 76, 60)
GREEN = (46, 204, 113)
YELLOW = (241, 196, 15)
PURPLE = (155, 89, 182)
ORANGE = (230, 126, 34)
GRAY = (127, 140, 141)
LIGHT_GRAY = (236, 240, 241)

# Game choices
class Choice(Enum):
    ROCK = "rock"
    PAPER = "paper"
    SCISSORS = "scissors"

class Particle:
    def __init__(self, x, y, color, velocity):
        self.x = x
        self.y = y
        self.color = color
        self.velocity = velocity
        self.life = 1.0
        self.size = random.randint(2, 6)
    
    def update(self):
        self.x += self.velocity[0]
        self.y += self.velocity[1]
        self.velocity = (self.velocity[0] * 0.98, self.velocity[1] + 0.2)  # Gravity
        self.life -= 0.02
        self.size = max(1, int(self.size * 0.99))
    
    def draw(self, screen):
        if self.life > 0:
            alpha = int(255 * self.life)
            color_with_alpha = (*self.color, alpha)
            
            # Create surface with alpha
            particle_surface = pygame.Surface((self.size * 2, self.size * 2), pygame.SRCALPHA)
            pygame.draw.circle(particle_surface, color_with_alpha, (self.size, self.size), self.size)
            screen.blit(particle_surface, (self.x - self.size, self.y - self.size))

class RockPaperScissorsGame:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("🎮 Rock Paper Scissors - Ultimate Edition")
        self.clock = pygame.time.Clock()
        
        # Fonts
        self.font_large = pygame.font.Font(None, 72)
        self.font_medium = pygame.font.Font(None, 48)
        self.font_small = pygame.font.Font(None, 36)
        self.font_tiny = pygame.font.Font(None, 24)
        
        # Game state
        self.player_score = 0
        self.computer_score = 0
        self.round_count = 0
        self.max_rounds = 5
        self.game_state = "menu"  # menu, playing, result, final
        self.player_choice = None
        self.computer_choice = None
        self.result_text = ""
        self.countdown = 0
        self.animation_timer = 0
        
        # Particles
        self.particles = []
        
        # Choice positions and sizes
        self.choice_positions = {
            Choice.ROCK: (200, 400),
            Choice.PAPER: (500, 400),
            Choice.SCISSORS: (800, 400)
        }
        self.choice_size = 120
        
        # Animation variables
        self.rock_angle = 0
        self.paper_wave = 0
        self.scissors_spin = 0
        
        # Sound effects
        self.init_sounds()
        
    def init_sounds(self):
        """Initialize sound effects using pygame mixer"""
        try:
            pygame.mixer.init(frequency=22050, size=-16, channels=2, buffer=512)
            self.sound_enabled = True
        except:
            self.sound_enabled = False
    
    def play_sound(self, sound_type):
        """Play sound effects"""
        if not self.sound_enabled:
            return
        
        try:
            if sound_type == "click":
                # Create click sound
                duration = 0.1
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 2000 * math.sin(2 * math.pi * 800 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            
            elif sound_type == "win":
                # Create win sound
                duration = 0.5
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq1 = 523 + (i * 100 / frames)  # Rising tone
                    freq2 = 659 + (i * 100 / frames)
                    wave1 = 1500 * math.sin(2 * math.pi * freq1 * i / sample_rate)
                    wave2 = 1500 * math.sin(2 * math.pi * freq2 * i / sample_rate)
                    wave = int((wave1 + wave2) * (1 - i / frames))
                    arr.append([wave, wave])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            
            elif sound_type == "lose":
                # Create lose sound
                duration = 0.8
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 300 - (i * 150 / frames)  # Descending tone
                    wave = 2000 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
        except:
            pass
    
    def create_particles(self, x, y, color, count=15):
        """Create particle explosion effect"""
        for _ in range(count):
            velocity = (
                random.uniform(-8, 8),
                random.uniform(-12, -3)
            )
            self.particles.append(Particle(x, y, color, velocity))
    
    def update_particles(self):
        """Update and remove dead particles"""
        self.particles = [p for p in self.particles if p.life > 0]
        for particle in self.particles:
            particle.update()
    
    def draw_particles(self):
        """Draw all particles"""
        for particle in self.particles:
            particle.draw(self.screen)
    
    def draw_choice(self, choice, x, y, size, selected=False, computer=False):
        """Draw animated choice icons"""
        # Update animations
        self.rock_angle += 2
        self.paper_wave += 0.15
        self.scissors_spin += 3
        
        # Selection glow
        if selected:
            glow_color = GREEN if not computer else RED
            pygame.draw.circle(self.screen, glow_color, (int(x), int(y)), size//2 + 15, 5)
            self.create_particles(x, y, glow_color, 3)
        
        # Background circle
        circle_color = LIGHT_GRAY if not selected else (WHITE if not computer else YELLOW)
        pygame.draw.circle(self.screen, circle_color, (int(x), int(y)), size//2)
        pygame.draw.circle(self.screen, DARK_BLUE, (int(x), int(y)), size//2, 3)
        
        if choice == Choice.ROCK:
            # Animated rock (rotating)
            points = []
            center_x, center_y = x, y
            radius = size // 3
            for i in range(8):
                angle = (i * 45 + self.rock_angle) * math.pi / 180
                point_radius = radius + random.randint(-2, 2) if selected else radius
                px = center_x + point_radius * math.cos(angle)
                py = center_y + point_radius * math.sin(angle)
                points.append((px, py))
            
            pygame.draw.polygon(self.screen, GRAY, points)
            pygame.draw.polygon(self.screen, BLACK, points, 3)
            
        elif choice == Choice.PAPER:
            # Animated paper (waving)
            paper_width = size // 2
            paper_height = size // 1.5
            wave_amplitude = 8 if selected else 4
            
            points = []
            for i in range(20):
                progress = i / 19
                wave_offset = wave_amplitude * math.sin(self.paper_wave + progress * math.pi * 2)
                px = x - paper_width//2 + progress * paper_width
                py = y - paper_height//2 + wave_offset
                points.append((px, py))
            
            for i in range(19, -1, -1):
                progress = i / 19
                wave_offset = wave_amplitude * math.sin(self.paper_wave + progress * math.pi * 2)
                px = x - paper_width//2 + progress * paper_width
                py = y + paper_height//2 + wave_offset
                points.append((px, py))
            
            pygame.draw.polygon(self.screen, WHITE, points)
            pygame.draw.polygon(self.screen, BLACK, points, 3)
            
        elif choice == Choice.SCISSORS:
            # Animated scissors (spinning)
            center_x, center_y = x, y
            blade_length = size // 3
            
            # Rotation angle
            angle = self.scissors_spin * math.pi / 180
            
            # First blade
            x1 = center_x + blade_length * math.cos(angle)
            y1 = center_y + blade_length * math.sin(angle)
            x2 = center_x + blade_length * 0.3 * math.cos(angle + math.pi/6)
            y2 = center_y + blade_length * 0.3 * math.sin(angle + math.pi/6)
            
            pygame.draw.line(self.screen, DARK_BLUE, (center_x, center_y), (x1, y1), 8)
            pygame.draw.line(self.screen, DARK_BLUE, (center_x, center_y), (x2, y2), 6)
            
            # Second blade
            x3 = center_x + blade_length * math.cos(angle + math.pi)
            y3 = center_y + blade_length * math.sin(angle + math.pi)
            x4 = center_x + blade_length * 0.3 * math.cos(angle + math.pi - math.pi/6)
            y4 = center_y + blade_length * 0.3 * math.sin(angle + math.pi - math.pi/6)
            
            pygame.draw.line(self.screen, DARK_BLUE, (center_x, center_y), (x3, y3), 8)
            pygame.draw.line(self.screen, DARK_BLUE, (center_x, center_y), (x4, y4), 6)
            
            # Center pivot
            pygame.draw.circle(self.screen, YELLOW, (int(center_x), int(center_y)), 8)
            pygame.draw.circle(self.screen, BLACK, (int(center_x), int(center_y)), 8, 2)
    
    def draw_vs_animation(self):
        """Draw animated VS in the center"""
        center_x, center_y = WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 50
        
        # Pulsating VS text
        pulse = abs(math.sin(pygame.time.get_ticks() * 0.01))
        scale = 1.0 + pulse * 0.3
        
        vs_font = pygame.font.Font(None, int(96 * scale))
        vs_text = vs_font.render("VS", True, YELLOW)
        vs_rect = vs_text.get_rect(center=(center_x, center_y))
        
        # Glow effect
        for offset in range(5, 0, -1):
            glow_surface = pygame.Surface(vs_text.get_size(), pygame.SRCALPHA)
            glow_text = vs_font.render("VS", True, (*YELLOW, 50))
            for dx in [-offset, 0, offset]:
                for dy in [-offset, 0, offset]:
                    if dx != 0 or dy != 0:
                        glow_surface.blit(glow_text, (dx, dy))
            self.screen.blit(glow_surface, (vs_rect.x - offset, vs_rect.y - offset))
        
        self.screen.blit(vs_text, vs_rect)
    
    def draw_menu(self):
        """Draw main menu"""
        self.screen.fill(DARK_BLUE)
        
        # Title with glow effect
        title_text = self.font_large.render("ROCK PAPER SCISSORS", True, WHITE)
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH//2, 150))
        
        # Title glow
        for offset in range(3, 0, -1):
            glow_text = self.font_large.render("ROCK PAPER SCISSORS", True, BLUE)
            self.screen.blit(glow_text, (title_rect.x - offset, title_rect.y - offset))
            self.screen.blit(glow_text, (title_rect.x + offset, title_rect.y + offset))
        
        self.screen.blit(title_text, title_rect)
        
        # Subtitle
        subtitle_text = self.font_medium.render("Ultimate Edition", True, YELLOW)
        subtitle_rect = subtitle_text.get_rect(center=(WINDOW_WIDTH//2, 220))
        self.screen.blit(subtitle_text, subtitle_rect)
        
        # Preview choices
        for i, choice in enumerate([Choice.ROCK, Choice.PAPER, Choice.SCISSORS]):
            x = 200 + i * 300
            y = 350
            self.draw_choice(choice, x, y, 100)
            
            # Choice names
            choice_name = choice.value.upper()
            name_text = self.font_small.render(choice_name, True, WHITE)
            name_rect = name_text.get_rect(center=(x, y + 80))
            self.screen.blit(name_text, name_rect)
        
        # Instructions
        instructions = [
            "Click on your choice to start playing!",
            f"First to {self.max_rounds} wins!",
            "Press ESC to quit"
        ]
        
        for i, instruction in enumerate(instructions):
            text = self.font_tiny.render(instruction, True, LIGHT_GRAY)
            text_rect = text.get_rect(center=(WINDOW_WIDTH//2, 500 + i * 30))
            self.screen.blit(text, text_rect)
    
    def draw_game(self):
        """Draw game screen"""
        self.screen.fill(DARK_BLUE)
        
        # Score display
        score_bg = pygame.Rect(0, 0, WINDOW_WIDTH, 100)
        pygame.draw.rect(self.screen, BLACK, score_bg)
        
        # Player score
        player_text = self.font_medium.render(f"PLAYER: {self.player_score}", True, GREEN)
        self.screen.blit(player_text, (50, 30))
        
        # Computer score
        computer_text = self.font_medium.render(f"COMPUTER: {self.computer_score}", True, RED)
        computer_rect = computer_text.get_rect()
        computer_rect.topright = (WINDOW_WIDTH - 50, 30)
        self.screen.blit(computer_text, computer_rect)
        
        # Round counter
        round_text = self.font_small.render(f"Round {self.round_count + 1}/{self.max_rounds}", True, WHITE)
        round_rect = round_text.get_rect(center=(WINDOW_WIDTH//2, 50))
        self.screen.blit(round_text, round_rect)
        
        # Draw VS animation
        self.draw_vs_animation()
        
        # Draw choices
        for choice in [Choice.ROCK, Choice.PAPER, Choice.SCISSORS]:
            x, y = self.choice_positions[choice]
            selected = (choice == self.player_choice)
            self.draw_choice(choice, x, y, self.choice_size, selected)
            
            # Choice labels
            label_text = self.font_small.render(choice.value.upper(), True, WHITE)
            label_rect = label_text.get_rect(center=(x, y + self.choice_size//2 + 30))
            self.screen.blit(label_text, label_rect)
        
        # Computer choice area
        if self.computer_choice:
            comp_x, comp_y = WINDOW_WIDTH//2, 200
            self.draw_choice(self.computer_choice, comp_x, comp_y, self.choice_size, True, True)
            
            comp_label = self.font_small.render("COMPUTER CHOSE", True, RED)
            comp_rect = comp_label.get_rect(center=(comp_x, comp_y - self.choice_size//2 - 40))
            self.screen.blit(comp_label, comp_rect)
        
        # Result text
        if self.result_text:
            result_color = GREEN if "WIN" in self.result_text else RED if "LOSE" in self.result_text else YELLOW
            result_surface = self.font_medium.render(self.result_text, True, result_color)
            result_rect = result_surface.get_rect(center=(WINDOW_WIDTH//2, 550))
            
            # Result glow
            for offset in range(2, 0, -1):
                glow_surface = self.font_medium.render(self.result_text, True, (*result_color, 100))
                self.screen.blit(glow_surface, (result_rect.x - offset, result_rect.y - offset))
                self.screen.blit(glow_surface, (result_rect.x + offset, result_rect.y + offset))
            
            self.screen.blit(result_surface, result_rect)
        
        # Instructions
        if not self.computer_choice:
            instruction = self.font_tiny.render("Click your choice!", True, WHITE)
            instruction_rect = instruction.get_rect(center=(WINDOW_WIDTH//2, 600))
            self.screen.blit(instruction, instruction_rect)
    
    def draw_final_result(self):
        """Draw final game result"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(200)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Determine winner
        if self.player_score > self.computer_score:
            title = "🏆 YOU WIN! 🏆"
            title_color = GREEN
            self.create_particles(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 - 100, GREEN, 30)
        elif self.computer_score > self.player_score:
            title = "💻 COMPUTER WINS! 💻"
            title_color = RED
        else:
            title = "🤝 IT'S A TIE! 🤝"
            title_color = YELLOW
        
        # Title
        title_surface = self.font_large.render(title, True, title_color)
        title_rect = title_surface.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 - 100))
        self.screen.blit(title_surface, title_rect)
        
        # Final scores
        final_score = f"Final Score: Player {self.player_score} - {self.computer_score} Computer"
        score_surface = self.font_medium.render(final_score, True, WHITE)
        score_rect = score_surface.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2))
        self.screen.blit(score_surface, score_rect)
        
        # Instructions
        restart_text = self.font_small.render("Press SPACE to play again or ESC to quit", True, LIGHT_GRAY)
        restart_rect = restart_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 100))
        self.screen.blit(restart_text, restart_rect)
    
    def get_winner(self, player, computer):
        """Determine the winner"""
        if player == computer:
            return "tie"
        
        winning_combinations = {
            (Choice.ROCK, Choice.SCISSORS): "player",
            (Choice.PAPER, Choice.ROCK): "player", 
            (Choice.SCISSORS, Choice.PAPER): "player"
        }
        
        return winning_combinations.get((player, computer), "computer")
    
    def handle_click(self, pos):
        """Handle mouse clicks"""
        if self.game_state == "menu":
            # Check if clicked on a choice
            for choice, (x, y) in self.choice_positions.items():
                distance = math.sqrt((pos[0] - x)**2 + (pos[1] - y)**2)
                if distance <= self.choice_size // 2:
                    self.start_round(choice)
                    break
        
        elif self.game_state == "playing" and not self.computer_choice:
            # Check if clicked on a choice
            for choice, (x, y) in self.choice_positions.items():
                distance = math.sqrt((pos[0] - x)**2 + (pos[1] - y)**2)
                if distance <= self.choice_size // 2:
                    self.start_round(choice)
                    break
    
    def start_round(self, player_choice):
        """Start a new round"""
        self.player_choice = player_choice
        self.computer_choice = random.choice(list(Choice))
        self.game_state = "result"
        
        self.play_sound("click")
        
        # Create particles at player choice
        x, y = self.choice_positions[player_choice]
        self.create_particles(x, y, GREEN, 20)
        
        # Determine winner
        winner = self.get_winner(self.player_choice, self.computer_choice)
        
        if winner == "player":
            self.player_score += 1
            self.result_text = "YOU WIN THIS ROUND!"
            self.play_sound("win")
        elif winner == "computer":
            self.computer_score += 1
            self.result_text = "COMPUTER WINS THIS ROUND!"
            self.play_sound("lose")
        else:
            self.result_text = "IT'S A TIE!"
        
        self.round_count += 1
        self.animation_timer = pygame.time.get_ticks()
        
        # Check if game is over
        if self.round_count >= self.max_rounds:
            pygame.time.set_timer(pygame.USEREVENT + 1, 3000)  # Show final result after 3 seconds
    
    def reset_game(self):
        """Reset game to initial state"""
        self.player_score = 0
        self.computer_score = 0
        self.round_count = 0
        self.game_state = "menu"
        self.player_choice = None
        self.computer_choice = None
        self.result_text = ""
        self.particles = []
    
    def run(self):
        """Main game loop"""
        running = True
        
        while running:
            # Handle events
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                
                elif event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE:
                        if self.game_state == "final":
                            running = False
                        else:
                            self.game_state = "menu"
                    elif event.key == pygame.K_SPACE:
                        if self.game_state == "final":
                            self.reset_game()
                        elif self.game_state == "result":
                            self.game_state = "playing"
                            self.player_choice = None
                            self.computer_choice = None
                            self.result_text = ""
                
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1:  # Left click
                        self.handle_click(event.pos)
                
                elif event.type == pygame.USEREVENT + 1:
                    self.game_state = "final"
                    pygame.time.set_timer(pygame.USEREVENT + 1, 0)  # Cancel timer
            
            # Auto-advance from result to next round
            if self.game_state == "result" and pygame.time.get_ticks() - self.animation_timer > 3000:
                if self.round_count < self.max_rounds:
                    self.game_state = "playing"
                    self.player_choice = None
                    self.computer_choice = None
                    self.result_text = ""
            
            # Update particles
            self.update_particles()
            
            # Draw everything
            if self.game_state == "menu":
                self.draw_menu()
            elif self.game_state in ["playing", "result"]:
                self.draw_game()
            elif self.game_state == "final":
                self.draw_game()
                self.draw_final_result()
            
            # Draw particles on top
            self.draw_particles()
            
            pygame.display.flip()
            self.clock.tick(FPS)
        
        pygame.quit()
        sys.exit()

def main():
    print("🎮 Rock Paper Scissors - Ultimate Edition")
    print("=" * 40)
    print("Features:")
    print("• Animated choices with visual effects")
    print("• Particle explosions and glow effects")
    print("• Sound effects for actions")
    print("• Best of 5 rounds tournament")
    print("• Professional game interface")
    print("\nControls:")
    print("• Click on choices to play")
    print("• SPACE: Continue/Play Again")
    print("• ESC: Quit/Back to Menu")
    print("\nStarting game...")
    
    try:
        game = RockPaperScissorsGame()
        game.run()
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()