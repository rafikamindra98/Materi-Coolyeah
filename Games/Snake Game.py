import pygame
import random
import sys
from enum import Enum
from collections import namedtuple
import math

# Initialize Pygame
pygame.init()

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (213, 50, 80)
GREEN = (0, 255, 0)
BLUE = (50, 153, 213)
YELLOW = (255, 255, 102)
PURPLE = (128, 0, 128)
ORANGE = (255, 165, 0)
DARK_GREEN = (0, 128, 0)
GRAY = (128, 128, 128)
LIGHT_GRAY = (192, 192, 192)

# Game settings
WINDOW_WIDTH = 800
WINDOW_HEIGHT = 600
GRID_SIZE = 20
GRID_WIDTH = WINDOW_WIDTH // GRID_SIZE
GRID_HEIGHT = WINDOW_HEIGHT // GRID_SIZE

# Fonts
font_large = pygame.font.Font(None, 48)
font_medium = pygame.font.Font(None, 36)
font_small = pygame.font.Font(None, 24)

# Direction enum
class Direction(Enum):
    RIGHT = 1
    LEFT = 2
    UP = 3
    DOWN = 4

Point = namedtuple('Point', 'x, y')

class SnakeGame:
    def __init__(self):
        self.display = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption('Snake Game - Python Edition')
        self.clock = pygame.time.Clock()
        
        # Game state
        self.direction = Direction.RIGHT
        self.head = Point(GRID_WIDTH//2, GRID_HEIGHT//2)
        self.snake = [self.head,
                     Point(self.head.x-1, self.head.y),
                     Point(self.head.x-2, self.head.y)]
        
        self.score = 0
        self.high_score = self.load_high_score()
        self.food = None
        self.special_food = None
        self.special_food_timer = 0
        self.game_over = False
        self.paused = False
        
        # Effects
        self.particles = []
        self.food_glow = 0
        
        self._place_food()
        
        # Sound effects (optional - create simple beeps)
        self.sound_enabled = True
        
    def load_high_score(self):
        try:
            with open('snake_high_score.txt', 'r') as f:
                return int(f.read())
        except:
            return 0
    
    def save_high_score(self):
        try:
            with open('snake_high_score.txt', 'w') as f:
                f.write(str(self.high_score))
        except:
            pass
    
    def play_sound(self, sound_type):
        if not self.sound_enabled:
            return
        
        # Simple sound effects using pygame mixer
        try:
            if sound_type == 'eat':
                # Create eating sound
                duration = 0.1
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 4096 * math.sin(2 * math.pi * 440 * i / sample_rate)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
            elif sound_type == 'game_over':
                # Create game over sound
                duration = 0.5
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 220 - (i * 200 / frames)  # Descending tone
                    wave = 2048 * math.sin(2 * math.pi * freq * i / sample_rate)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
        except:
            pass  # Sound not available
    
    def _place_food(self):
        while True:
            x = random.randint(0, GRID_WIDTH-1)
            y = random.randint(0, GRID_HEIGHT-1)
            self.food = Point(x, y)
            if self.food not in self.snake:
                break
    
    def _place_special_food(self):
        if self.special_food is None and random.randint(1, 100) <= 10:  # 10% chance
            while True:
                x = random.randint(0, GRID_WIDTH-1)
                y = random.randint(0, GRID_HEIGHT-1)
                self.special_food = Point(x, y)
                if self.special_food not in self.snake and self.special_food != self.food:
                    self.special_food_timer = 300  # 5 seconds at 60 FPS
                    break
    
    def _move(self, direction):
        x = self.head.x
        y = self.head.y
        
        if direction == Direction.RIGHT:
            x += 1
        elif direction == Direction.LEFT:
            x -= 1
        elif direction == Direction.DOWN:
            y += 1
        elif direction == Direction.UP:
            y -= 1
        
        self.head = Point(x, y)
    
    def _is_collision(self, point=None):
        if point is None:
            point = self.head
        
        # Boundary collision
        if point.x > GRID_WIDTH-1 or point.x < 0 or point.y > GRID_HEIGHT-1 or point.y < 0:
            return True
        
        # Self collision
        if point in self.snake[1:]:
            return True
        
        return False
    
    def _create_particles(self, x, y, color):
        for _ in range(10):
            particle = {
                'x': x * GRID_SIZE + GRID_SIZE // 2,
                'y': y * GRID_SIZE + GRID_SIZE // 2,
                'dx': random.randint(-5, 5),
                'dy': random.randint(-5, 5),
                'life': 30,
                'color': color
            }
            self.particles.append(particle)
    
    def _update_particles(self):
        for particle in self.particles[:]:
            particle['x'] += particle['dx']
            particle['y'] += particle['dy']
            particle['life'] -= 1
            particle['dy'] += 0.2  # Gravity
            
            if particle['life'] <= 0:
                self.particles.remove(particle)
    
    def _draw_particles(self):
        for particle in self.particles:
            alpha = particle['life'] / 30.0
            size = max(1, int(3 * alpha))
            color = (*particle['color'], int(255 * alpha))
            
            # Create a surface with alpha
            particle_surf = pygame.Surface((size*2, size*2), pygame.SRCALPHA)
            pygame.draw.circle(particle_surf, color, (size, size), size)
            self.display.blit(particle_surf, (particle['x']-size, particle['y']-size))
    
    def play_step(self):
        # Handle events
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return True
            
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_UP and self.direction != Direction.DOWN:
                    self.direction = Direction.UP
                elif event.key == pygame.K_DOWN and self.direction != Direction.UP:
                    self.direction = Direction.DOWN
                elif event.key == pygame.K_LEFT and self.direction != Direction.RIGHT:
                    self.direction = Direction.LEFT
                elif event.key == pygame.K_RIGHT and self.direction != Direction.LEFT:
                    self.direction = Direction.RIGHT
                elif event.key == pygame.K_SPACE:
                    if self.game_over:
                        self.reset_game()
                    else:
                        self.paused = not self.paused
                elif event.key == pygame.K_r and self.game_over:
                    self.reset_game()
                elif event.key == pygame.K_ESCAPE:
                    return True
        
        if self.game_over or self.paused:
            return False
        
        # Move snake
        self._move(self.direction)
        
        # Check collision
        if self._is_collision():
            self.game_over = True
            if self.score > self.high_score:
                self.high_score = self.score
                self.save_high_score()
            self.play_sound('game_over')
            return False
        
        # Insert new head
        self.snake.insert(0, self.head)
        
        # Check food collision
        reward = 0
        if self.head == self.food:
            self.score += 10
            reward = 10
            self.play_sound('eat')
            self._create_particles(self.food.x, self.food.y, GREEN)
            self._place_food()
        else:
            self.snake.pop()
        
        # Check special food collision
        if self.special_food and self.head == self.special_food:
            self.score += 50
            reward = 50
            self.play_sound('eat')
            self._create_particles(self.special_food.x, self.special_food.y, YELLOW)
            self.special_food = None
            self.special_food_timer = 0
        
        # Update special food timer
        if self.special_food_timer > 0:
            self.special_food_timer -= 1
            if self.special_food_timer == 0:
                self.special_food = None
        
        # Place special food randomly
        self._place_special_food()
        
        # Update effects
        self._update_particles()
        self.food_glow = (self.food_glow + 5) % 360
        
        return False
    
    def reset_game(self):
        self.direction = Direction.RIGHT
        self.head = Point(GRID_WIDTH//2, GRID_HEIGHT//2)
        self.snake = [self.head,
                     Point(self.head.x-1, self.head.y),
                     Point(self.head.x-2, self.head.y)]
        self.score = 0
        self.food = None
        self.special_food = None
        self.special_food_timer = 0
        self.game_over = False
        self.paused = False
        self.particles = []
        self._place_food()
    
    def _draw_grid(self):
        for x in range(0, WINDOW_WIDTH, GRID_SIZE):
            pygame.draw.line(self.display, (20, 20, 20), (x, 0), (x, WINDOW_HEIGHT))
        for y in range(0, WINDOW_HEIGHT, GRID_SIZE):
            pygame.draw.line(self.display, (20, 20, 20), (0, y), (WINDOW_WIDTH, y))
    
    def _draw_snake(self):
        for i, point in enumerate(self.snake):
            rect = pygame.Rect(point.x * GRID_SIZE, point.y * GRID_SIZE, GRID_SIZE, GRID_SIZE)
            
            if i == 0:  # Head
                pygame.draw.rect(self.display, DARK_GREEN, rect)
                pygame.draw.rect(self.display, GREEN, rect, 3)
                
                # Draw eyes
                eye_size = 3
                if self.direction == Direction.RIGHT:
                    eye1 = (rect.centerx + 3, rect.centery - 3)
                    eye2 = (rect.centerx + 3, rect.centery + 3)
                elif self.direction == Direction.LEFT:
                    eye1 = (rect.centerx - 3, rect.centery - 3)
                    eye2 = (rect.centerx - 3, rect.centery + 3)
                elif self.direction == Direction.UP:
                    eye1 = (rect.centerx - 3, rect.centery - 3)
                    eye2 = (rect.centerx + 3, rect.centery - 3)
                else:  # DOWN
                    eye1 = (rect.centerx - 3, rect.centery + 3)
                    eye2 = (rect.centerx + 3, rect.centery + 3)
                
                pygame.draw.circle(self.display, RED, eye1, eye_size)
                pygame.draw.circle(self.display, RED, eye2, eye_size)
            else:  # Body
                brightness = 1.0 - (i * 0.1)
                color = (int(0 * brightness), int(200 * brightness), int(0 * brightness))
                pygame.draw.rect(self.display, color, rect)
                pygame.draw.rect(self.display, GREEN, rect, 2)
    
    def _draw_food(self):
        # Regular food with glow effect
        food_rect = pygame.Rect(self.food.x * GRID_SIZE, self.food.y * GRID_SIZE, GRID_SIZE, GRID_SIZE)
        
        # Glow effect
        glow_intensity = abs(math.sin(math.radians(self.food_glow))) * 50 + 50
        glow_color = (255, int(glow_intensity), int(glow_intensity))
        
        pygame.draw.circle(self.display, glow_color, food_rect.center, GRID_SIZE//2 + 2)
        pygame.draw.circle(self.display, RED, food_rect.center, GRID_SIZE//2)
        pygame.draw.circle(self.display, WHITE, (food_rect.centerx - 3, food_rect.centery - 3), 3)
        
        # Special food
        if self.special_food:
            special_rect = pygame.Rect(self.special_food.x * GRID_SIZE, self.special_food.y * GRID_SIZE, GRID_SIZE, GRID_SIZE)
            
            # Blinking effect
            blink_intensity = abs(math.sin(pygame.time.get_ticks() * 0.02)) * 255
            special_color = (255, 255, int(blink_intensity))
            
            pygame.draw.circle(self.display, special_color, special_rect.center, GRID_SIZE//2 + 3)
            pygame.draw.circle(self.display, YELLOW, special_rect.center, GRID_SIZE//2)
            pygame.draw.circle(self.display, WHITE, special_rect.center, GRID_SIZE//4)
    
    def _draw_ui(self):
        # Score
        score_text = font_medium.render(f'Score: {self.score}', True, WHITE)
        self.display.blit(score_text, (10, 10))
        
        # High Score
        high_score_text = font_small.render(f'High Score: {self.high_score}', True, YELLOW)
        self.display.blit(high_score_text, (10, 50))
        
        # Length
        length_text = font_small.render(f'Length: {len(self.snake)}', True, LIGHT_GRAY)
        self.display.blit(length_text, (10, 80))
        
        # Special food timer
        if self.special_food:
            timer_text = font_small.render(f'Special Food: {self.special_food_timer // 60 + 1}s', True, YELLOW)
            self.display.blit(timer_text, (WINDOW_WIDTH - 200, 10))
    
    def _draw_game_over(self):
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(128)
        overlay.fill(BLACK)
        self.display.blit(overlay, (0, 0))
        
        # Game Over text
        game_over_text = font_large.render('GAME OVER', True, RED)
        game_over_rect = game_over_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 - 60))
        self.display.blit(game_over_text, game_over_rect)
        
        # Final score
        final_score_text = font_medium.render(f'Final Score: {self.score}', True, WHITE)
        final_score_rect = final_score_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 - 20))
        self.display.blit(final_score_text, final_score_rect)
        
        # High score
        if self.score == self.high_score and self.score > 0:
            new_record_text = font_medium.render('NEW HIGH SCORE!', True, YELLOW)
            new_record_rect = new_record_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 20))
            self.display.blit(new_record_text, new_record_rect)
        
        # Instructions
        restart_text = font_small.render('Press SPACE or R to restart', True, LIGHT_GRAY)
        restart_rect = restart_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 60))
        self.display.blit(restart_text, restart_rect)
        
        quit_text = font_small.render('Press ESC to quit', True, LIGHT_GRAY)
        quit_rect = quit_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 90))
        self.display.blit(quit_text, quit_rect)
    
    def _draw_pause(self):
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(128)
        overlay.fill(BLACK)
        self.display.blit(overlay, (0, 0))
        
        # Pause text
        pause_text = font_large.render('PAUSED', True, YELLOW)
        pause_rect = pause_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2))
        self.display.blit(pause_text, pause_rect)
        
        # Instructions
        continue_text = font_small.render('Press SPACE to continue', True, LIGHT_GRAY)
        continue_rect = continue_text.get_rect(center=(WINDOW_WIDTH//2, WINDOW_HEIGHT//2 + 40))
        self.display.blit(continue_text, continue_rect)
    
    def update_ui(self):
        self.display.fill(BLACK)
        
        # Draw grid
        self._draw_grid()
        
        # Draw game elements
        self._draw_food()
        self._draw_snake()
        
        # Draw particles
        self._draw_particles()
        
        # Draw UI
        self._draw_ui()
        
        # Draw overlays
        if self.game_over:
            self._draw_game_over()
        elif self.paused:
            self._draw_pause()
        
        pygame.display.flip()

def main():
    game = SnakeGame()
    
    # Game loop
    while True:
        game_exit = game.play_step()
        
        if game_exit:
            break
        
        game.update_ui()
        game.clock.tick(15)  # 15 FPS for smooth gameplay
    
    pygame.quit()
    sys.exit()

if __name__ == '__main__':
    print("🐍 Snake Game - Python Edition")
    print("=" * 30)
    print("Controls:")
    print("• Arrow Keys - Move snake")
    print("• SPACE - Pause/Resume/Restart")
    print("• R - Restart (when game over)")
    print("• ESC - Quit game")
    print("\nFeatures:")
    print("• Regular food (+10 points)")
    print("• Special food (+50 points) - appears randomly")
    print("• High score tracking")
    print("• Particle effects")
    print("• Sound effects")
    print("\nStarting game...")
    
    try:
        main()
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
        pygame.quit()
        sys.exit()
