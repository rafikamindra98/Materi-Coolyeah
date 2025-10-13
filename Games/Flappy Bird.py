import pygame
import random
import sys
import math
import json
import os
from enum import Enum

# Initialize Pygame
pygame.init()

# Constants
WINDOW_WIDTH = 800
WINDOW_HEIGHT = 600
FPS = 60

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
BLUE = (135, 206, 235)
GREEN = (34, 139, 34)
YELLOW = (255, 255, 0)
RED = (255, 0, 0)
ORANGE = (255, 165, 0)
PURPLE = (128, 0, 128)
GRAY = (128, 128, 128)
LIGHT_BLUE = (173, 216, 230)
DARK_GREEN = (0, 100, 0)

# Game settings
GRAVITY = 0.5
BIRD_JUMP = -10
PIPE_SPEED = 3
PIPE_WIDTH = 80
PIPE_GAP = 200
PIPE_SPAWN_DELAY = 90  # frames

class GameState(Enum):
    MENU = "menu"
    PLAYING = "playing"
    GAME_OVER = "game_over"
    PAUSED = "paused"

class Particle:
    def __init__(self, x, y, color, velocity, life=60):
        self.x = x
        self.y = y
        self.color = color
        self.velocity = velocity
        self.life = life
        self.max_life = life
        self.size = random.randint(2, 5)
    
    def update(self):
        self.x += self.velocity[0]
        self.y += self.velocity[1]
        self.velocity = (self.velocity[0] * 0.98, self.velocity[1] + 0.1)
        self.life -= 1
        
    def draw(self, screen):
        if self.life > 0:
            alpha = int(255 * (self.life / self.max_life))
            color_with_alpha = (*self.color, alpha)
            size = max(1, int(self.size * (self.life / self.max_life)))
            
            particle_surface = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            pygame.draw.circle(particle_surface, color_with_alpha, (size, size), size)
            screen.blit(particle_surface, (self.x - size, self.y - size))

class Bird:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.velocity = 0
        self.size = 25
        self.angle = 0
        self.flap_animation = 0
        
    def update(self):
        self.velocity += GRAVITY
        self.y += self.velocity
        
        # Update angle based on velocity
        self.angle = min(90, max(-30, self.velocity * 3))
        
        # Wing flap animation
        self.flap_animation += 0.3
        
    def flap(self):
        self.velocity = BIRD_JUMP
        
    def draw(self, screen):
        # Bird body
        bird_color = YELLOW if self.velocity < 0 else ORANGE
        
        # Wing flap effect
        wing_offset = abs(math.sin(self.flap_animation)) * 5
        
        # Draw bird shadow
        pygame.draw.ellipse(screen, GRAY, 
                          (self.x - self.size//2 + 2, self.y - self.size//2 + 2, 
                           self.size, self.size))
        
        # Draw bird body
        pygame.draw.ellipse(screen, bird_color, 
                          (self.x - self.size//2, self.y - self.size//2, 
                           self.size, self.size))
        
        # Draw wing
        wing_color = ORANGE if self.velocity < 0 else RED
        wing_rect = (self.x - self.size//4, self.y - wing_offset, self.size//2, self.size//3)
        pygame.draw.ellipse(screen, wing_color, wing_rect)
        
        # Draw eye
        eye_x = self.x + 3
        eye_y = self.y - 5
        pygame.draw.circle(screen, WHITE, (int(eye_x), int(eye_y)), 6)
        pygame.draw.circle(screen, BLACK, (int(eye_x + 2), int(eye_y)), 3)
        
        # Draw beak
        beak_points = [
            (self.x + self.size//2 - 5, self.y),
            (self.x + self.size//2 + 10, self.y - 3),
            (self.x + self.size//2 + 10, self.y + 3)
        ]
        pygame.draw.polygon(screen, ORANGE, beak_points)
        
    def get_rect(self):
        return pygame.Rect(self.x - self.size//2, self.y - self.size//2, self.size, self.size)

class Pipe:
    def __init__(self, x, gap_y):
        self.x = x
        self.gap_y = gap_y
        self.gap_size = PIPE_GAP
        self.width = PIPE_WIDTH
        self.scored = False
        
    def update(self):
        self.x -= PIPE_SPEED
        
    def draw(self, screen):
        # Top pipe
        top_pipe_height = self.gap_y - self.gap_size // 2
        if top_pipe_height > 0:
            # Pipe body
            top_rect = pygame.Rect(self.x, 0, self.width, top_pipe_height)
            pygame.draw.rect(screen, DARK_GREEN, top_rect)
            pygame.draw.rect(screen, BLACK, top_rect, 3)
            
            # Pipe cap
            cap_rect = pygame.Rect(self.x - 10, top_pipe_height - 30, self.width + 20, 30)
            pygame.draw.rect(screen, GREEN, cap_rect)
            pygame.draw.rect(screen, BLACK, cap_rect, 3)
        
        # Bottom pipe
        bottom_pipe_top = self.gap_y + self.gap_size // 2
        bottom_pipe_height = WINDOW_HEIGHT - bottom_pipe_top
        if bottom_pipe_height > 0:
            # Pipe body
            bottom_rect = pygame.Rect(self.x, bottom_pipe_top, self.width, bottom_pipe_height)
            pygame.draw.rect(screen, DARK_GREEN, bottom_rect)
            pygame.draw.rect(screen, BLACK, bottom_rect, 3)
            
            # Pipe cap
            cap_rect = pygame.Rect(self.x - 10, bottom_pipe_top, self.width + 20, 30)
            pygame.draw.rect(screen, GREEN, cap_rect)
            pygame.draw.rect(screen, BLACK, cap_rect, 3)
            
    def get_rects(self):
        top_pipe_height = self.gap_y - self.gap_size // 2
        bottom_pipe_top = self.gap_y + self.gap_size // 2
        
        top_rect = pygame.Rect(self.x, 0, self.width, top_pipe_height)
        bottom_rect = pygame.Rect(self.x, bottom_pipe_top, self.width, WINDOW_HEIGHT - bottom_pipe_top)
        
        return [top_rect, bottom_rect]
        
    def is_off_screen(self):
        return self.x + self.width < 0

class Cloud:
    def __init__(self, x, y, size):
        self.x = x
        self.y = y
        self.size = size
        self.speed = random.uniform(0.5, 1.5)
        
    def update(self):
        self.x -= self.speed
        if self.x + self.size < 0:
            self.x = WINDOW_WIDTH + random.randint(0, 200)
            self.y = random.randint(50, 200)
            
    def draw(self, screen):
        # Draw fluffy cloud
        cloud_color = WHITE
        positions = [
            (self.x, self.y),
            (self.x + self.size//3, self.y - self.size//4),
            (self.x + self.size//2, self.y),
            (self.x + self.size//1.5, self.y - self.size//6),
            (self.x + self.size, self.y)
        ]
        
        for pos in positions:
            pygame.draw.circle(screen, cloud_color, (int(pos[0]), int(pos[1])), self.size//4)

class FlappyBirdGame:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("🐦 Flappy Bird - Ultimate Edition")
        self.clock = pygame.time.Clock()
        
        # Fonts
        self.font_large = pygame.font.Font(None, 72)
        self.font_medium = pygame.font.Font(None, 48)
        self.font_small = pygame.font.Font(None, 36)
        self.font_tiny = pygame.font.Font(None, 24)
        
        # Game objects
        self.bird = Bird(100, WINDOW_HEIGHT // 2)
        self.pipes = []
        self.clouds = []
        self.particles = []
        
        # Initialize clouds
        for i in range(5):
            cloud = Cloud(random.randint(0, WINDOW_WIDTH), 
                         random.randint(50, 200), 
                         random.randint(40, 80))
            self.clouds.append(cloud)
        
        # Game state
        self.game_state = GameState.MENU
        self.score = 0
        self.high_score = self.load_high_score()
        self.pipe_timer = 0
        self.ground_offset = 0
        
        # Sound system
        self.sound_enabled = True
        self.init_sounds()
        
    def init_sounds(self):
        """Initialize sound effects"""
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
            if sound_type == "flap":
                duration = 0.1
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 2000 * math.sin(2 * math.pi * 800 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "score":
                duration = 0.3
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 523 + (i * 200 / frames)
                    wave = 1500 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "hit":
                duration = 0.5
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 200 - (i * 150 / frames)
                    wave = 3000 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
        except:
            pass
    
    def load_high_score(self):
        """Load high score from file"""
        try:
            with open('flappy_high_score.json', 'r') as f:
                data = json.load(f)
                return data.get('high_score', 0)
        except:
            return 0
    
    def save_high_score(self):
        """Save high score to file"""
        try:
            with open('flappy_high_score.json', 'w') as f:
                json.dump({'high_score': self.high_score}, f)
        except:
            pass
    
    def create_particles(self, x, y, color, count=10):
        """Create particle effects"""
        for _ in range(count):
            velocity = (random.uniform(-5, 5), random.uniform(-8, -2))
            self.particles.append(Particle(x, y, color, velocity))
    
    def update_particles(self):
        """Update particles"""
        self.particles = [p for p in self.particles if p.life > 0]
        for particle in self.particles:
            particle.update()
    
    def draw_particles(self):
        """Draw particles"""
        for particle in self.particles:
            particle.draw(self.screen)
    
    def spawn_pipe(self):
        """Spawn new pipe"""
        gap_y = random.randint(150, WINDOW_HEIGHT - 150)
        pipe = Pipe(WINDOW_WIDTH, gap_y)
        self.pipes.append(pipe)
    
    def check_collisions(self):
        """Check for collisions"""
        bird_rect = self.bird.get_rect()
        
        # Check ground and ceiling
        if self.bird.y <= 0 or self.bird.y >= WINDOW_HEIGHT - 50:
            return True
        
        # Check pipe collisions
        for pipe in self.pipes:
            pipe_rects = pipe.get_rects()
            for rect in pipe_rects:
                if bird_rect.colliderect(rect):
                    return True
        
        return False
    
    def update_score(self):
        """Update score when passing pipes"""
        for pipe in self.pipes:
            if not pipe.scored and pipe.x + pipe.width < self.bird.x:
                pipe.scored = True
                self.score += 1
                self.play_sound("score")
                self.create_particles(self.bird.x, self.bird.y, YELLOW, 15)
                
                if self.score > self.high_score:
                    self.high_score = self.score
    
    def reset_game(self):
        """Reset game to initial state"""
        self.bird = Bird(100, WINDOW_HEIGHT // 2)
        self.pipes = []
        self.particles = []
        self.score = 0
        self.pipe_timer = 0
        self.game_state = GameState.PLAYING
    
    def draw_background(self):
        """Draw animated background"""
        # Sky gradient
        for y in range(WINDOW_HEIGHT - 50):
            color_ratio = y / (WINDOW_HEIGHT - 50)
            r = int(135 + (173 - 135) * color_ratio)
            g = int(206 + (216 - 206) * color_ratio)
            b = int(235 + (230 - 235) * color_ratio)
            pygame.draw.line(self.screen, (r, g, b), (0, y), (WINDOW_WIDTH, y))
        
        # Draw clouds
        for cloud in self.clouds:
            cloud.update()
            cloud.draw(self.screen)
        
        # Ground
        self.ground_offset = (self.ground_offset + 2) % 40
        ground_rect = pygame.Rect(0, WINDOW_HEIGHT - 50, WINDOW_WIDTH, 50)
        pygame.draw.rect(self.screen, GREEN, ground_rect)
        pygame.draw.rect(self.screen, DARK_GREEN, ground_rect, 3)
        
        # Ground pattern
        for x in range(-self.ground_offset, WINDOW_WIDTH, 40):
            pygame.draw.line(self.screen, DARK_GREEN, (x, WINDOW_HEIGHT - 50), (x, WINDOW_HEIGHT), 2)
    
    def draw_menu(self):
        """Draw main menu"""
        # Title with animation
        title_bounce = abs(math.sin(pygame.time.get_ticks() * 0.003)) * 10
        title_text = self.font_large.render("FLAPPY BIRD", True, WHITE)
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH//2, 150 + title_bounce))
        
        # Title shadow
        shadow_text = self.font_large.render("FLAPPY BIRD", True, BLACK)
        shadow_rect = shadow_text.get_rect(center=(WINDOW_WIDTH//2 + 3, 153 + title_bounce))
        self.screen.blit(shadow_text, shadow_rect)
        self.screen.blit(title_text, title_rect)
        
        # Animated bird preview
        preview_bird = Bird(WINDOW_WIDTH//2, 250 + math.sin(pygame.time.get_ticks() * 0.005) * 20)
        preview_bird.flap_animation = pygame.time.get_ticks() * 0.01
        preview_bird.draw(self.screen)
        
        # Instructions
        instructions = [
            "Press SPACE or CLICK to flap",
            "Avoid the pipes!",
            f"High Score: {self.high_score}",
            "",
            "Press SPACE to start"
        ]
        
        for i, instruction in enumerate(instructions):
            color = YELLOW if i == len(instructions) - 1 else WHITE
            font = self.font_small if i < len(instructions) - 1 else self.font_medium
            text = font.render(instruction, True, color)
            text_rect = text.get_rect(center=(WINDOW_WIDTH//2, 350 + i * 40))
            self.screen.blit(text, text_rect)
    
    def draw_game(self):
        """Draw game screen"""
        # Draw game objects
        self.bird.draw(self.screen)
        
        for pipe in self.pipes:
            pipe.draw(self.screen)
        
        # Score display
        score_text = self.font_medium.render(f"Score: {self.score}", True, WHITE)
        score_shadow = self.font_medium.render(f"Score: {self.score}", True, BLACK)
        self.screen.blit(score_shadow, (22, 22))
        self.screen.blit(score_text, (20, 20))
        
        # High score
        if self.score > 0:
            high_score_text = self.font_small.render(f"Best: {self.high_score}", True, YELLOW)
            self.screen.blit(high_score_text, (20, 70))
        
        # Instructions
        if self.score == 0 and len(self.pipes) == 0:
            instruction = self.font_small.render("Press SPACE to flap!", True, WHITE)
            instruction_rect = instruction.get_rect(center=(WINDOW_WIDTH//2, 100))
            instruction_shadow = self.font_small.render("Press SPACE to flap!", True, BLACK)
            shadow_rect = instruction_shadow.get_rect(center=(WINDOW_WIDTH//2 + 2, 102))
            self.screen.blit(instruction_shadow, shadow_rect)
            self.screen.blit(instruction, instruction_rect)
    
    def draw_game_over(self):
        """Draw game over screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(150)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Game Over text
        game_over_text = self.font_large.render("GAME OVER", True, RED)
        game_over_rect = game_over_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 - 100))
        
        # Text shadow
        shadow_text = self.font_large.render("GAME OVER", True, BLACK)
        shadow_rect = shadow_text.get_rect(center=(WINDOW_WIDTH//2 + 3, WINDOW_HEIGHT//2 - 97))
        self.screen.blit(shadow_text, shadow_rect)
        self.screen.blit(game_over_text, game_over_rect)
        
        # Final score
        final_score_text = self.font_medium.render(f"Final Score: {self.score}", True, WHITE)
        final_score_rect = final_score_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 - 30))
        self.screen.blit(final_score_text, final_score_rect)
        
        # High score
        if self.score == self.high_score and self.score > 0:
            new_record_text = self.font_small.render("NEW HIGH SCORE!", True, YELLOW)
            new_record_rect = new_record_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 10))
            self.screen.blit(new_record_text, new_record_rect)
        else:
            high_score_text = self.font_small.render(f"High Score: {self.high_score}", True, YELLOW)
            high_score_rect = high_score_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 10))
            self.screen.blit(high_score_text, high_score_rect)
        
        # Instructions
        restart_text = self.font_small.render("Press SPACE to restart", True, WHITE)
        restart_rect = restart_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 60))
        self.screen.blit(restart_text, restart_rect)
        
        menu_text = self.font_small.render("Press ESC for menu", True, WHITE)
        menu_rect = menu_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 90))
        self.screen.blit(menu_text, menu_rect)
    
    def draw_pause(self):
        """Draw pause screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(100)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Pause text
        pause_text = self.font_large.render("PAUSED", True, YELLOW)
        pause_rect = pause_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2))
        
        # Text shadow
        shadow_text = self.font_large.render("PAUSED", True, BLACK)
        shadow_rect = shadow_text.get_rect(center=(WINDOW_WIDTH//2 + 2, WINDOW_HEIGHT//2 + 2))
        self.screen.blit(shadow_text, shadow_rect)
        self.screen.blit(pause_text, pause_rect)
        
        # Instructions
        continue_text = self.font_small.render("Press P to continue", True, WHITE)
        continue_rect = continue_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 60))
        self.screen.blit(continue_text, continue_rect)
    
    def handle_events(self):
        """Handle game events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    if self.game_state == GameState.PLAYING:
                        self.game_state = GameState.MENU
                    elif self.game_state == GameState.GAME_OVER:
                        self.game_state = GameState.MENU
                    elif self.game_state == GameState.MENU:
                        return False
                
                elif event.key == pygame.K_SPACE:
                    if self.game_state == GameState.MENU:
                        self.reset_game()
                    elif self.game_state == GameState.PLAYING:
                        self.bird.flap()
                        self.play_sound("flap")
                        self.create_particles(self.bird.x - 20, self.bird.y, WHITE, 5)
                    elif self.game_state == GameState.GAME_OVER:
                        self.reset_game()
                
                elif event.key == pygame.K_p:
                    if self.game_state == GameState.PLAYING:
                        self.game_state = GameState.PAUSED
                    elif self.game_state == GameState.PAUSED:
                        self.game_state = GameState.PLAYING
            
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:  # Left click
                    if self.game_state == GameState.MENU:
                        self.reset_game()
                    elif self.game_state == GameState.PLAYING:
                        self.bird.flap()
                        self.play_sound("flap")
                        self.create_particles(self.bird.x - 20, self.bird.y, WHITE, 5)
                    elif self.game_state == GameState.GAME_OVER:
                        self.reset_game()
        
        return True
    
    def update(self):
        """Update game state"""
        if self.game_state == GameState.PLAYING:
            # Update bird
            self.bird.update()
            
            # Update pipes
            for pipe in self.pipes[:]:
                pipe.update()
                if pipe.is_off_screen():
                    self.pipes.remove(pipe)
            
            # Spawn new pipes
            self.pipe_timer += 1
            if self.pipe_timer >= PIPE_SPAWN_DELAY:
                self.spawn_pipe()
                self.pipe_timer = 0
            
            # Check collisions
            if self.check_collisions():
                self.game_state = GameState.GAME_OVER
                self.play_sound("hit")
                self.create_particles(self.bird.x, self.bird.y, RED, 20)
                self.save_high_score()
            
            # Update score
            self.update_score()
        
        # Always update particles
        self.update_particles()
    
    def draw(self):
        """Draw everything"""
        self.draw_background()
        
        if self.game_state == GameState.MENU:
            self.draw_menu()
        elif self.game_state in [GameState.PLAYING, GameState.PAUSED]:
            self.draw_game()
            if self.game_state == GameState.PAUSED:
                self.draw_pause()
        elif self.game_state == GameState.GAME_OVER:
            self.draw_game()
            self.draw_game_over()
        
        # Draw particles on top
        self.draw_particles()
        
        pygame.display.flip()
    
    def run(self):
        """Main game loop"""
        running = True
        
        while running:
            running = self.handle_events()
            self.update()
            self.draw()
            self.clock.tick(FPS)
        
        pygame.quit()
        sys.exit()

def main():
    print("🐦 Flappy Bird - Ultimate Edition")
    print("=" * 35)
    print("Features:")
    print("• Smooth bird animations with wing flapping")
    print("• Animated background with moving clouds")
    print("• Particle effects for flapping and scoring")
    print("• Sound effects for actions")
    print("• High score tracking (saved to file)")
    print("• Pause functionality")
    print("• Professional graphics and UI")
    print("\nControls:")
    print("• SPACE/CLICK - Flap wings")
    print("• P - Pause/Resume")
    print("• ESC - Menu/Quit")
    print("\nStarting game...")
    
    try:
        game = FlappyBirdGame()
        game.run()
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()